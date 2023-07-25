classdef InstrumentsData < handle
    % This m-file defines the instruments data part of an inversion object.
    % It encapsulates all of the information that is associated with a set
    % of inversion instruments (regardless of their type).
    %
    % PUBLIC PROPERTIES:
    %   -> indices: object of the indices inversion data class
    %   -> values: object of the values inversion data class
    %   -> weights: object of the weights inversion data class
    %   -> reflectedIndices: object of the indices inversion data class
    %   -> reflectedValues: object of the values inversion data class      
    %   -> numberPerPeriod: row vector with number of intsruments per 
    %      period
    %   -> totalNumber: non-negative numeric integer
    %   -> reflectedNumberPerPeriod: row vector with number of shocks not 
    %      being used as instruments per period
    %   -> reflectedTotalNumber: non-negative numeric integer  
    %   -> horizon: maximum horizon up to which instruments data is set    
    %   -> isWeightsNonEmpty: true/false   
    %
    % EVENTS:
    %   -> instrumentsDataUpdated: broadcast if the state of the object 
    %      changes
    %
    % PUBLIC METHODS:
    %   -> none
    %
    % DETAILS:
    %   -> This m-file defines an instruments inversion data object as part
    %      of an Inversion.Instructions object. It cannot be instantiated 
    %      outside of the Inversion.Instruments class and has no public 
    %      methods other than "get" and "set" methods.
    %   -> It encapsulates all the information around inversion instruments
    %      data and exposes the means to set inversion instruments data (as
    %      defined by indices for the instruments being used, and, 
    %      optionally, weights for those targets).  
    %   -> Given that information, one important function this class
    %      performs is to compute the existing values of the instruments
    %      being used in the inversion. It also computes the indices of the
    %      shocks not being used in the inversion and their values. These
    %      data are an important part of the MAPS LSS model inversion 
    %      algorithm.
    %   -> This class also manages the validation of the instruments data,
    %      ensuring that any weights set are consistent in dimension with
    %      the indices.
    %   -> Notice that one possible state of this object set on
    %      construction is "islockedForEditing=true". This is important
    %      because it facilitates the construction of an "empty" object for
    %      either anticipated or "unanticipated" instruments and all of the
    %      associated object dependent properties without allowing users to
    %      set any data. This logic is dictated by the logic of the
    %      inversion algorithm itself which, for performance reasons,
    %      proceeds on the basis that all data is always present in the
    %      inversion instructions.
    %
    % NOTES:
    %   -> None. 
    %
    % This version: 25/11/2013
    % Author(s): Matt Waldron    
    
    %% PROPERTIES
    properties (Dependent=true)
        indices
        values
        weights
        reflectedIndices
        reflectedValues
        numberPerPeriod
        totalNumber
        reflectedNumberPerPeriod
        reflectedTotalNumber
        horizon
        isWeightsNonEmpty     
    end
    properties (SetAccess=private)
        isLockedForEditing
    end
    properties (Access=private)
        pIndices
        pWeights
        pReflectedIndices
        initialisedReflectedIndsCell
        nz
        H
        isValidated
        existingShocks
    end  
    %% EVENTS
    events
        instrumentsDataUpdated
    end
    %% PUBLIC METHODS
    methods
        %% SET METHOD FOR INDICES
        function set.indices(iInstrumentsDataObj,iCell)
            if iInstrumentsDataObj.isLockedForEditing
               errId = ['MAPS:',mfilename('class'),':IllegalSet'];
               generate_and_throw_MAPS_exception(errId);
            end
            iInstrumentsDataObj.pIndices.cellArray = iCell;
            iInstrumentsDataObj.reflectInstrumentIndices();
            iInstrumentsDataObj.updateFlagAndNotifyForValidation();
        end    
        %% SET METHOD FOR WEIGHTS
        function set.weights(iInstrumentsDataObj,iCell)
            if iInstrumentsDataObj.isLockedForEditing
               errId = ['MAPS:',mfilename('class'),':IllegalSet'];
               generate_and_throw_MAPS_exception(errId);
            end
            iInstrumentsDataObj.pWeights.cellArray = iCell;
            iInstrumentsDataObj.updateFlagAndNotifyForValidation();
        end
        %% GET METHOD FOR INDICES
        function ziIndSet = get.indices(iInstrumentsDataObj)
            ziIndSet = iInstrumentsDataObj.pIndices.cellArray;
        end
        %% GET METHOD FOR VALUES
        function zibarSet = get.values(iInstrumentsDataObj)
            ziIndSet = iInstrumentsDataObj.indices;
            zibarSet = ...
                iInstrumentsDataObj.computeInstrumentValues(ziIndSet);
        end
        %% GET METHOD FOR WEIGHTS
        function ziWeightSet = get.weights(iInstrumentsDataObj)
            ziWeightSet = iInstrumentsDataObj.pWeights.cellArray;
        end
        %% GET METHOD FOR REFLECTED INDICES
        function znIndSet = get.reflectedIndices(iInstrumentsDataObj)
            znIndSet = iInstrumentsDataObj.pReflectedIndices.cellArray;
        end
        %% GET METHOD FOR REFLECTED VALUES
        function znbarSet = get.reflectedValues(iInstrumentsDataObj)
            znIndSet = iInstrumentsDataObj.reflectedIndices;
            znbarSet = ...
                iInstrumentsDataObj.computeInstrumentValues(znIndSet);
        end
        %% GET METHOD FOR NUMBER PER PERIOD
        function nPerPeriod = get.numberPerPeriod(iInstrumentsDataObj)
            nPerPeriod = iInstrumentsDataObj.pIndices.numberPerPeriod;
        end
        %% GET METHOD FOR TOTAL NUMBER
        function nTotal = get.totalNumber(iInstrumentsDataObj)
            nTotal = sum(iInstrumentsDataObj.numberPerPeriod);
        end
        %% GET METHOD FOR REFLECTED NUMBER PER PERIOD
        function nPerPeriod = ...
                get.reflectedNumberPerPeriod(iInstrumentsDataObj)
            nPerPeriod = ...
                iInstrumentsDataObj.pReflectedIndices.numberPerPeriod;
        end
        %% GET METHOD FOR TOTAL NUMBER
        function nTotal = get.reflectedTotalNumber(iInstrumentsDataObj)
            nTotal = sum(iInstrumentsDataObj.reflectedNumberPerPeriod);
        end
        %% GET METHOD FOR HORIZON
        function horizon = get.horizon(iInstrumentsDataObj)
           horizon = find(iInstrumentsDataObj.numberPerPeriod>0,1,'last'); 
           if isempty(horizon)
               horizon = 0;
           end
        end
        %% GET METHOD FOR WEIGHTS FLAG
        function isWeightsNonEmpty = ...
                get.isWeightsNonEmpty(iInstrumentsDataObj)
            isWeightsNonEmpty = iInstrumentsDataObj.pWeights.isNonEmpty;
        end
    end    
    %% PRIVATE METHODS TO INVERSION.INSTRUCTIONS OBJECTS
    methods (Access={?Inversion.Instruments})
        %% CONSTRUCTOR
        function iInstrumentsDataObj = InstrumentsData(...
                zbar,isOpenForEditing)
            % This is the constructor method for Instruments data class.
            %
            % INPUTS:
            %   -> zbar: existing shock values
            %   -> isOpenForEditing: true/false
            %
            % OUTPUTS:
            %   -> iInstrumentsDataObj: instance of the inversion 
            %      instruments data object
            %
            %% VALIDATE INPUT SHOCKS MATRIX
            iInstrumentsDataObj.validateInputShocksMatrix(zbar);
            iInstrumentsDataObj.existingShocks = zbar;
            %% GET DIMENSIONS OF SHOCKS MATRIX INPUT
            % This facilitates the creation of the reflected instruments
            % data.
            [nShocks,horizon] = size(zbar);
            iInstrumentsDataObj.nz = nShocks;
            iInstrumentsDataObj.H = horizon;
            %% INITIALISE THE INDICES PROPERTY
            iInstrumentsDataObj.pIndices = ...
                Inversion.Data.Indices(horizon,nShocks);
            %% INITIALISE THE WEIGHTS OBJECT
            iInstrumentsDataObj.pWeights = ...
                Inversion.Data.Weights(horizon);
            %% INITIALISE REFLECTED INDICES OBJECT
            % Note that the initialised state of the reflected indices
            % object is stored as a property in its own right. It is then
            % used for computation of the reflected indices after the
            % indices have been set (which saves having to recompute it).
            % Note also that information about the reflected indices is
            % stored as a structure on this object (rather than another
            % instance of the inversion indices data object to avoid
            % paying the cost of validating the reflected indices data,
            % which this class guarantees will always be in a valid state).
            initReflectedIndsCell = ...
                Inversion.InstrumentsData.initialiseReflectedIndices(...
                horizon,nShocks);
            iInstrumentsDataObj.initialisedReflectedIndsCell = ...
                initReflectedIndsCell;
            iInstrumentsDataObj.pReflectedIndices.cellArray = ...
                initReflectedIndsCell;
            iInstrumentsDataObj.pReflectedIndices.numberPerPeriod = ...
                nShocks*ones(1,horizon);
            %% SET VALIDATION & LOCK FLAGS
            iInstrumentsDataObj.isValidated = true;
            iInstrumentsDataObj.isLockedForEditing = ~isOpenForEditing;
        end
        %% METHOD TO UPDATE EXISTING SHOCK VALUES
        function updateExistingShockValues(iInstrumentsDataObj,zbarUpdated)
            % This method contains the logic to updates the shock values.
            %
            % INPUTS:
            %   -> zbarUpdated: new shock values
            %
            % OUTPUTS:
            %   -> iInstrumentsDataObj: none
            %
            %% VALIDATE SHOCKS MATRIX
            iInstrumentsDataObj.validateInputShocksMatrix(zbarUpdated);
            %% CHECK THAT DIMENSIONS ARE CONSISTENT WITH THOSE ON CONSTRUCT
            if any(size(iInstrumentsDataObj.existingShocks)-...
                    size(zbarUpdated))
               errId = [...
                    'MAPS:',mfilename('class'),':InvalidUpdateShockDims'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% UPDATE SHOCK VALUES
            iInstrumentsDataObj.existingShocks = zbarUpdated;
        end
        %% VALIDATION METHOD
        function validate(iInstrumentsDataObj)
            % This is the validation method for the Instruments data class.
            % It validates that the user input indices and any weights
            % are consistent with each other.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% EXIT IF ALREADY VALIDATED
            if iInstrumentsDataObj.isValidated
                return
            end
            %% VALIDATE THAT INDICES & WEIGHTS HAVE SAME DIMENSION
            if iInstrumentsDataObj.isWeightsNonEmpty
                if any(iInstrumentsDataObj.numberPerPeriod-...
                        iInstrumentsDataObj.pWeights.numberPerPeriod)
                    errId = ['MAPS:',...
                        mfilename('class'),':InvalidIndWeightsDims'];
                    generate_and_throw_MAPS_exception(errId);
                end
            end
            %% RESET VALIDATION FLAG IF NO EXCEPTIONS WERE FOUND
            iInstrumentsDataObj.isValidated = true;
        end
    end
    %% PRIVATE METHODS
    methods (Access=private)
        %% METHOD TO COMPUTE INSTRUMENT VALUES
        function zjbarSet = ...
                computeInstrumentValues(iInstrumentsDataObj,zjIndSet)
            % Method to compute shock values associated with indices.
            % Note that the indices are passed in an input so that the same
            % method can be used to compute instrument shock values and
            % "reflected instrument" values - i.e. values of the shocks not
            % being used as instruments.
            %
            % INPUTS:
            %   -> zjIndSet: row cell array of indices
            %
            % OUTPUTS:
            %   -> zjbarSet: row cell arry of shock values
            %
            %% INITIALISE VALUES
            zjbarSet = cell(1,iInstrumentsDataObj.H);
            %% COMPUTE VALUES PERIOD-BY-PERIOD
            for t = 1:iInstrumentsDataObj.H
                zjbarSet{t} = ...
                    iInstrumentsDataObj.existingShocks(zjIndSet{t},t);
            end
        end   
        %% METHOD TO REFLECT INSTRUMENT INDICES
        function reflectInstrumentIndices(iInstrumentsDataObj)   
            % Method to reflect the instrument indices.
            % This method computes the indices of all the shocks not being
            % used as instruments on a period-by-period basis.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% EXTRACT INITIALISED REFLECTED INDICES
            znIndSet = iInstrumentsDataObj.initialisedReflectedIndsCell;
            %% EXTRACT INSTRUMENT INDICES
            ziIndSet = iInstrumentsDataObj.indices;
            %% COMPUTE REFLECTED INDICES PERIOD-BY-PERIOD
            for t = 1:iInstrumentsDataObj.horizon
                znIndSet{t}(ziIndSet{t}) = [];
                znIndSet{t} = znIndSet{t}(:);
            end
            %% SET CELL ARRAY FIELD OF STRUCTURE
            iInstrumentsDataObj.pReflectedIndices.cellArray = znIndSet;
            iInstrumentsDataObj.pReflectedIndices.numberPerPeriod = ...
                iInstrumentsDataObj.nz*ones(1,iInstrumentsDataObj.H)-...
                iInstrumentsDataObj.numberPerPeriod;
        end 
        %% METHOD TO VALIDATE INPUT SHOCKS MATRIX
        function validateInputShocksMatrix(~,zbar)
            % This method validates input shock matrices.
            %
            % INPUTS:
            %   -> zbar: matrix of shock values
            %
            % OUTPUTS:
            %   -> none
            %
            %% VALIDATE THAT THE INPUT MATRIX IS TWO-DIM, FINITE & REAL
            if ~is_finite_real_two_dimensional_numeric_matrix(zbar)
                errId = ['MAPS:',mfilename('class'),':InvalidShocksMat'];
                generate_and_throw_MAPS_exception(errId);
            end
        end
        %% METHOD TO UPDATE VALIDATION FLAG & NOTIFY AS STATE CHANGES
        function updateFlagAndNotifyForValidation(iInstrumentsDataObj)
            % Method to handle changes in state of data object.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% UPDATE VALIDATION FLAG
            iInstrumentsDataObj.isValidated = false;
            %% BROADCAST INSTRUMENTS DATA STATE CHANGE EVENT
            notify(iInstrumentsDataObj,'instrumentsDataUpdated');
        end
    end
    %% PRIVATE STATIC METHODS
    methods (Static=true,Access=private)
        %% METHOD TO INITIALISE REFLECTED SHOCK INDICES
        function znIndSetInit = initialiseReflectedIndices(H,nz)
            % This static helper initialises reflected instrument indices.
            % In the initialised state of the object, there are no
            % instruments so this is initialised to the indices of all the
            % shocks in every period.
            %
            % INPUTS:
            %   -> H: forecast horizon
            %   -> nz: number of shocks
            %
            % OUTPUTS:
            %   -> znIndSetInit: initialised reflected indices
            %
            %% CREATE COMPLETE VECTOR OF SHOCK INDICES
            zInd = (1:nz)';
            %% INITIALISE THE REFLECTED INDICES CELL ARRAY
            znIndSetInit = cell(1,H);
            %% INITIALISE THE REFLECTED INDICES PERIOD-BY-PERIOD
            for h = 1:H
                znIndSetInit{h} = zInd;
            end
        end
    end
end