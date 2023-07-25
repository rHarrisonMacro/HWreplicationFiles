classdef Targets < handle
    % This m-file defines the targets part of an inversion object.
    % It encapsulates all of the information that is associated with a set
    % of inversion targets.
    %
    % PUBLIC PROPERTIES:
    %   -> indices: object of the indices inversion data class
    %   -> values: object of the values inversion data class
    %   -> weights: object of the weights inversion data class
    %   -> numberPerPeriod: row vector with number of targets per period
    %   -> totalNumber: non-negative numeric integer
    %   -> horizon: maximum horizon up to which targets data is set
    %   -> isWeightsNonEmpty: true/false
    %
    % EVENTS:
    %   -> targetsUpdated: broadcast if the state of the object changes
    %
    % PUBLIC METHODS:
    %   -> none
    %
    % DETAILS:
    %   -> This m-file defines a targets inversion data object as part of
    %      an Inversion.Instructions object. It cannot be instantiated 
    %      outside of that object and has no public methods other than 
    %      "get" and "set" methods.
    %   -> It encapsulates all the information around inversion targets and
    %      exposes the means to set inversion target data (as defined by 
    %      indices for the endogenous variables being targeted, values for
    %      those targets and, optionally, weights for those targets).  
    %   -> As part of that, it manages the validation of the object.
    %      Specifically, it checks that the values, indices and weights 
    %      that users have set are consistent in dimension with each
    %      other.
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
        numberPerPeriod
        totalNumber
        horizon
        isWeightsNonEmpty
    end
    properties (Access=private)
        pIndices
        pValues
        pWeights
        isValidated
    end   
    %% EVENTS
    events
        targetsUpdated
    end
    %% PUBLIC METHODS
    methods
        %% SET METHOD FOR INDICES
        function set.indices(iTargetsObj,iCell)
            iTargetsObj.pIndices.cellArray = iCell;
            iTargetsObj.updateFlagAndNotifyForValidation();
        end
        %% SET METHOD FOR VALUES
        function set.values(iTargetsObj,iCell)
            iTargetsObj.pValues.cellArray = iCell;
            iTargetsObj.updateFlagAndNotifyForValidation();
        end        
        %% SET METHOD FOR WEIGHTS
        function set.weights(iTargetsObj,iCell)
            iTargetsObj.pWeights.cellArray = iCell;
            iTargetsObj.updateFlagAndNotifyForValidation();
        end
        %% GET METHOD FOR INDICES
        function xiIndSet = get.indices(iTargetsObj)
            xiIndSet = iTargetsObj.pIndices.cellArray;
        end
        %% GET METHOD FOR VALUES
        function xibarSet = get.values(iTargetsObj)
            xibarSet = iTargetsObj.pValues.cellArray;
        end
        %% GET METHOD FOR WEIGHTS
        function xiWeightSet = get.weights(iTargetsObj)
            xiWeightSet = iTargetsObj.pWeights.cellArray;
        end
        %% GET METHOD FOR NUMBER PER PERIOD
        function nPerPeriod = get.numberPerPeriod(iTargetsObj)
            nPerPeriod = iTargetsObj.pIndices.numberPerPeriod;
        end
        %% GET METHOD FOR TOTAL NUMBER
        function nTotal = get.totalNumber(iTargetsObj)
            nTotal = sum(iTargetsObj.numberPerPeriod);
        end
        %% GET METHOD FOR HORIZON
        function horizon = get.horizon(iTargetsObj)
           horizon = find(iTargetsObj.numberPerPeriod>0,1,'last');
           if isempty(horizon)
               horizon = 0;
           end
        end
        %% GET METHOD FOR IS WEIGHTS NON EMPTY FLAG
        function isWeightsNonEmpty = get.isWeightsNonEmpty(iTargetsObj)
            isWeightsNonEmpty = iTargetsObj.pWeights.isNonEmpty;
        end
    end    
    %% PRIVATE METHODS TO INVERSION.INSTRUCTIONS OBJECTS
    methods (Access={?Inversion.Instructions})
        %% CONSTRUCTOR
        function iTargetsObj = Targets(nx,H)
            % This is the constructor method for the Targets class.
            %
            % INPUTS:
            %   -> nx: number of endogenous variables
            %   -> H: forecast horizon
            %
            % OUTPUTS:
            %   -> iTargetsObj: instance of the inversion targets object
            %
            %% INITIALISE INDICES
            iTargetsObj.pIndices = Inversion.Data.Indices(H,nx);
            %% INITIALISE VALUES
            iTargetsObj.pValues = Inversion.Data.Values(H);
            %% INITIALISE WEIGHTS
            iTargetsObj.pWeights = Inversion.Data.Weights(H);
            %% INITIALISE VALIDATION FLAG
            iTargetsObj.isValidated = true;
        end
        %% VALIDATION METHOD
        function validate(iTargetsObj)
            % This is the validation method for the Targets class.
            % It validates that the user input indices, values and weights
            % are consistent with each other.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% EXIT IF ALREADY VALIDATED
            if iTargetsObj.isValidated
                return
            end
            %% VALIDATE THAT INDICES & VALUES HAVE SAME DIMENSION
            if any(iTargetsObj.pIndices.numberPerPeriod-...
                    iTargetsObj.pValues.numberPerPeriod)
                errId = ['MAPS:',mfilename('class'),':InvalidIndValsDims'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% VALIDATE THAT INDICES & WEIGHTS HAVE SAME DIMENSION
            if iTargetsObj.isWeightsNonEmpty
                if any(iTargetsObj.pIndices.numberPerPeriod-...
                        iTargetsObj.pWeights.numberPerPeriod)
                    errId = ['MAPS:',...
                        mfilename('class'),':InvalidIndWeightsDims'];
                    generate_and_throw_MAPS_exception(errId);
                end
            end
            %% RESET VALIDATION FLAG IF NO EXCEPTIONS WERE FOUND
            iTargetsObj.isValidated = true;
        end
    end
    %% PRIVATE ACCESS METHODS
    methods (Access=private)
        %% METHOD TO UPDATE VALIDATION FLAG & NOTIFY AS STATE CHANGES
        function updateFlagAndNotifyForValidation(iTargetsObj)
            % Method to handle changes in state of data objects.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% UPDATE VALIDATION FLAG
            iTargetsObj.isValidated = false;
            %% BROADCAST TARGETS STATE CHANGE EVENT
            notify(iTargetsObj,'targetsUpdated');
        end
    end
end