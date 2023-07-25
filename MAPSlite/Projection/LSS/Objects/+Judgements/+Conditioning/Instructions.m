classdef Instructions < handle
    % This m-file defines a conditioning judgements instructions object.
    % This object allows for instructions for an inversion to be set, 
    % either as part of a broader judgements instructions set or as an
    % object in its own right.
    %
    % PUBLIC PROPERTIES:
    %   -> Targets: conditioning targets object
    %   -> Instruments: conditioning instruments object
    %   -> isNonEmpty: true/false
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> constructor: requires a model & run data structure input and,
    %      optionally a logical to indicate that the targets data will be
    %      input in marginal space.
    %   -> applyJudgementToForecastRun: takes a model & run data structure
    %      as input and implements the inversion to update the forecast 
    %      shock values
    %   -> createInversionInstructions: takes a model & forecast run
    %      dataset as input and creates an inversion instructions object
    %   -> validate: takes a model & forecast run dataset as input and
    %      validates the conditioining instructions object
    %
    % DETAILS:
    %   -> This m-file defines a judgement conditioning instructions 
    %      object, which forms the basis of information for an inversion.
    %   -> It contains a set of targets and instruments, which are 
    %      themselves modelled as objects. See the 
    %      Judgements.Conditioning.Targets & 
    %      Judgements.Conditioning.Instruments classes for details. These 
    %      component objects can only be instantiated through this object.
    %   -> The object can be instantiated by passing in a valid MAPS LSS
    %      model, associated forecast run dataset and, optionally, a
    %      logical which dictates whether the judgement target data will be
    %      input as marginals or as levels (which is the default). See the
    %      Judgements.Conditioning.Targets class for more details.
    %   -> Once this object has been instantiated it exposes the Targets & 
    %      Instruments properties through "set" methods. These objects in
    %      turn expose a number of different methods for setting the
    %      judgement conditioning data. For example, it would be possible
    %      to set target data for a model variable called 'v' in a number
    %      of different ways, which would all achieve the same result:
    %           jConditioiningObj.Targets.ModelVariables 
    %               = {'v' [<dataRowVector>]}
    %           jConditioiningObj.Targets.ModelVariables...
    %               .add('v',[<dataRowVector>])
    %           jConditioiningObj.Targets.ModelVariables...
    %               .overwrite({'v' [<dataRowVector>}]) 
    %           jConditioiningObj.Targets.add({'v' [<dataRowVector>}])
    %           jConditioiningObj.Targets...
    %               .overwrite({'v' [<dataRowVector>}])
    %   -> Note that the interpretation of the numeric row vectors depends
    %      on whether the the targets are treated as marginals or levels on
    %      construction.
    %   -> And it is possible to set shock instruments in very similar ways
    %      (though it is necessary to always specify whether the shock is 
    %      anticipated or unanticipated - i.e. there are no methods on the
    %      Instruments class that are publicly accessible).  
    %   -> This object also contains the logic for the updating of a 
    %      forecast run dataset and, associated with that, creation of an 
    %      inversion instructions object from the conditioning information.
    %      See the LSS model impose judgement macro for details of when the
    %      run data structure is updated and see Inversion.Instructions for
    %      details of what this looks like.
    %   -> Prior to creation of the inversion instructions, this object is
    %      validated. The conditioing information is valid if: a) the 
    %      targets information is valid - see 
    %      Judgements.Conditioning.Targets for details; b) there is at
    %      least one instrument with which to condition the projection on 
    %      the targets; c) there is at least one target with which to 
    %      identify the instruments; d) there are no unanticipated shocks
    %      which cannot be identified (i.e. which cannot be used to 
    %      implement any of the target data points). 
    %   -> Notice that it is only necessary to validate the object if: a)
    %      there is something in it (i.e. isNonEmpty = true); b) the state
    %      of the object has changed since the previous validation. In the
    %      event of (b), it is necessary to validate the targets and the
    %      combination of the targets/instruments if the targets object
    %      has changed, but only the latter if the instruments object
    %      has changed. This object sets listeners for those events, which
    %      are broadcast from the instruments and targets objects.
    %
    % NOTES:
    %   -> Note the use of private properties for the target & instruments 
    %      objects. The technique is a useful one for exposing an interface
    %      to users, while controlling the execution on "set" and in being
    %      free to initialise properties without routing through the set 
    %      method.
    %   -> There is an awkwardness in validation of the judgements 
    %      conditioning object. This stems from the fact that the validity
    %      of the targets object is dependent on the state of the run 
    %      dataset - see the notes in the Judgements.Conditioning.Targets
    %      class for a brief discussion. Unfortunately, run datasets are 
    %      not objects so there is no way of guaranteeing that they are 
    %      valid or that they are consistent with the inputs used to 
    %      instantiate this object. The validation in this object works 
    %      around that by testing whether the RunData object has changed 
    %      since it was previously validated (or, more precisely, whether 
    %      particular parts of it have changed). This does not plug any of
    %      the validation holes.: for example, it is possible to
    %      instantiate the object with a different model and/or run data
    %      structure to that used in updating the forecast run, creating
    %      the inversion instructions or validating the object.
    %
    % This version: 25/11/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (Dependent=true)
        Targets
        Instruments
        Options
        isNonEmpty
    end
    properties (Access=private)
        pTargets
        pInstruments
        pOptions
        isCombinedTargetInstrumentSetValidated
    end    
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR
        function jConditioningObj = Instructions(...
                Model,RunData,defineAsMarginals)
            % This is the constructor method for the Conditioning class.
            %
            % INPUTS:
            %   -> Model: MAPS LSS model structure
            %   -> RunData: forecast run structure
            %   -> defineAsMarginals: true/false
            %
            % OUTPUTS:
            %   -> jConditioningObj: instance of the conditioning object
            %   
            %% CHECK INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadConstructNargin'];
                generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
            elseif ~isstruct(Model)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput1'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~isstruct(RunData)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput2'];
                generate_and_throw_MAPS_exception(errId);
            elseif nargin>2 && ~is_logical_scalar(defineAsMarginals)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput3'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% HANDLE OPTIONAL DEFINE AS MARGINALS INPUT
            if nargin < 3
                defineAsMarginals = false;
            end
            %% CHECK MODEL IS OF LSS CLASS
            % Note that this ensures that the input model is not an NLBL 
            % model. It is in no way a guarantee that the model is valid
            % since all we have established so far (in the cell above) is
            % that the input is a structure. There are two possible
            % validation holes: 1) the input is a non-model structure in
            % which case the unpack_model function will throw an exception;
            % 2) the input is a structure with many of the right
            % characteristics, but is not a valid model - this could lead
            % to unhandled run-time errors or unexpected results further
            % down-stream.
            isModelLSS = unpack_model(Model,'modelIsLinearStateSpace');
            if ~isModelLSS
                errId = ['MAPS:',mfilename('class'),':BadModelType'];
                generate_and_throw_MAPS_exception(errId);
            end  
            %% COMPUTE FORECAST HORIZON
            [PHI,F] = unpack_model(Model,{'PHI','F'});
            try
                [~,H] = interrogate_forecast_shocks_structure(...
                    RunData.Forecast.Shocks,PHI,F);
            catch ValidationE
                errId = ['MAPS:',mfilename('class'),':BadRunData'];
                generate_MAPS_exception_add_cause_and_throw(...
                    ValidationE,errId);
            end
            %% INITIALISE THE PRIVATE TARGETS PROPERTY
            jConditioningObj.pTargets = ...
                Judgements.Conditioning.Targets(...
                Model,RunData,defineAsMarginals);           
            %% INITIALISE THE PRIVATE INSTRUMENTS PROPERTY          
            jConditioningObj.pInstruments = ...
                Judgements.Conditioning.Instruments(Model,H);   
            %% INITIALISE THE PRIVATE OPTIONS PROPERTY          
            jConditioningObj.pOptions = Judgements.Conditioning.Options();             
            %% INITIALISE VALIDATION FLAGS
            % The initial state of the object is empty, so it begins in a
            % valid state.
            jConditioningObj.isCombinedTargetInstrumentSetValidated = true;
            %% ADD LISTENERS
            % A listener is set on the targets and instruments objects to 
            % listen to changes in state. This is used to update 
            % information about whether the targets & instruments needs to
            % be revalidated - see the validate method for details.
            addlistener(jConditioningObj.pTargets,...
                'jTargetsObjStateChanged',...
                @jConditioningObj.updateValidationFlag);
            addlistener(jConditioningObj.pInstruments,...
                'jInstrumentsObjStateChanged',...
                @jConditioningObj.updateValidationFlag);
        end
        %% UPDATE FORECAST RUN (INVERSION)
        function RunData = applyJudgementToForecastRun(...
                jConditioningObj,Model,RunData)
            % This method updates a forecast run through inversion.
            % It converts the conditioing information into an inversion
            % instructions object and then calls the invert method on that
            % object.
            %
            % INPUTS:
            %   -> Model: MAPS LSS model structure
            %   -> RunData: forecast run dataset structure
            %
            % OUTPUTS:
            %   -> RunData: updated forecast run dataset structure
            %
            %% CHECK NUMBER OF INPUTS
            if nargin < 3
                errId = ['MAPS:',mfilename('class'),...
                    ':BadApplyJudgementToForecastRunNargin'];
                generate_and_throw_MAPS_exception(...
                    errId,{num2str(nargin-1)});
            end            
            %% CREATE INVERSION INSTRUCTIONS OBJECT
            iInstructionsObj = jConditioningObj...
                .createInversionInstructions(Model,RunData);
            %% UNPACK MODEL SOLUTION MATRICES
            [B,PHI,F] = unpack_model(Model,{'B','PHI','F'});
            %% UNPACK MODEL VARIABLE INITIAL CONDITIONS FOR PROJECTION
            xT = RunData.Constraint.modelVariables;
            %% CALL INVERSION
            RunData.Forecast.Shocks = iInstructionsObj.invert(B,PHI,F,xT);
        end
        %% CREATE INVERSION INSTRUCTIONS METHOD
        function iInstructionsObj = ...
                createInversionInstructions(jConditioningObj,Model,RunData)
            % This method creates an inversion instructions object.
            % This object can be used directly in MAPS' LSS model inversion
            % functionality. This method will throw an exception if the
            % conditioning information is invalid.
            %
            % INPUTS:
            %   -> Model: MAPS LSS model structure
            %   -> RunData: forecast run dataset structure
            %
            % OUTPUTS:
            %   -> iInstructionsObj: inversion instructions object
            %
            %% CHECK NUMBER OF INPUTS
            if nargin < 3
                errId = ['MAPS:',mfilename('class'),...
                    ':BadCreateInversionInstructionsNargin'];
                generate_and_throw_MAPS_exception(...
                    errId,{num2str(nargin-1)});
            end
            %% VALIDATE THE CONDITIONING OBJECT
            jConditioningObj.validate(Model,RunData);
            %% INSTANTIATE INVERSION INSTRUCTIONS OBJECT
            iInstructionsObj = Inversion.Instructions(...
                Model,RunData.Forecast.Shocks);
            %% CREATE TARGETS INVERSION INFORMATION (IF APPLICABLE)
            if jConditioningObj.Targets.isNonEmpty
                [xiInds,xiVals] = Judgements.Conditioning.Instructions...
                    .createInversionCellArrays(...
                    jConditioningObj.Targets.AllVariables.inds,...
                    jConditioningObj.Targets.AllVariables.horizon,...
                    jConditioningObj.Targets.AllVariables.matLogicals,...
                    jConditioningObj.Targets.AllVariables.mat);
                iInstructionsObj.Targets.indices = xiInds;
                iInstructionsObj.Targets.values = xiVals;
            end
            %% CREATE INSTRUMENTS INVERSION INFORMATION (IF APPLICABLE)
            if jConditioningObj.Instruments.includesAnticipatedInstruments
                uiInds = Judgements.Conditioning.Instructions...
                    .createInversionCellArrays(...
                    jConditioningObj.Instruments.Anticipated.inds,...
                    jConditioningObj.Instruments.Anticipated.horizon,...
                    jConditioningObj.Instruments.Anticipated.mat);
                iInstructionsObj.Instruments.Anticipated.indices = uiInds;
            end
            if jConditioningObj.Instruments...
                    .includesUnanticipatedInstruments
                uiInds = Judgements.Conditioning.Instructions...
                    .createInversionCellArrays(...
                    jConditioningObj.Instruments.Unanticipated.inds,...
                    jConditioningObj.Instruments.Unanticipated.horizon,...
                    jConditioningObj.Instruments.Unanticipated.mat);
                iInstructionsObj.Instruments...
                    .Unanticipated.indices = uiInds;
            end
            %% ADD OPTIONS
            jConditioningOpts = properties(jConditioningObj.Options);
            nOpts = size(jConditioningOpts,1);
            for iOpt = 1:nOpts
                iInstructionsObj.Options.(jConditioningOpts{iOpt}) = ...
                    jConditioningObj.Options.(jConditioningOpts{iOpt});
            end            
        end
        %% VALIDATE METHOD      
        function validate(jConditioningObj,Model,RunData)
            % This method validates the conditioning object.
            % It will throw an exception in the event that any of the MAPS
            % LSS model inversion rules are broken.
            %
            % INPUTS:
            %   -> Model: MAPS LSS model structure
            %   -> RunData: forecast run dataset structure
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK INPUTS
            if nargin < 3
                errId = ['MAPS:',mfilename('class'),':BadValidateNargin'];
                generate_and_throw_MAPS_exception(...
                    errId,{num2str(nargin-1)});
            elseif ~isstruct(Model)
                errId = ['MAPS:',mfilename('class'),':BadValidateInput1'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~isstruct(RunData) 
                errId = ['MAPS:',mfilename('class'),':BadValidateInput2'];
                generate_and_throw_MAPS_exception(errId);            
            end
            %% SET VALIDATION FLAGS
            isTargetSetValid = true;
            isAnyTargetSet = true;
            isAnyInstrumentAvailable = true;          
            isUnanticipatedInstrumentSetEffective = true;            
            %% VALIDATE TARGETS
            % Note that the targets are always re-validated because changes
            % in the content of the run data structure could invalid 
            % previously validated targets data.  The targets validation
            % function keeps track of its own state and will only
            % revalidate if necessary.
            try
                jConditioningObj.Targets.validate(Model,RunData);
            catch TargetsE
                isTargetSetValid = false;
            end
            %% VALIDATE COMBINATION OF TARGETS & INSTRUMENTS (IF NECESSARY)
            % There are 3 possible errors: a) there are no targets to
            % identify the instruments; b) there are no instruments to use
            % to condition on the targets; c) one or more unanticipated 
            % shock instruments cannot be identified because their horizon
            % exceeds that of the targets.
            if ~jConditioningObj.isCombinedTargetInstrumentSetValidated
                if ~jConditioningObj.Targets.isNonEmpty && ...
                        jConditioningObj.Instruments.isNonEmpty
                    isAnyTargetSet = false;
                elseif jConditioningObj.Targets.isNonEmpty && ...
                        ~jConditioningObj.Instruments.isNonEmpty
                    isAnyInstrumentAvailable = false;
                else
                    tHorizon = jConditioningObj.Targets.horizon;
                    uiHorizon = ...
                        jConditioningObj.Instruments.Unanticipated.horizon;
                    if uiHorizon > tHorizon
                        isUnanticipatedInstrumentSetEffective = false;
                    end
                end
            end
            %% UPDATE VALIDATION FLAG
            jConditioningObj.isCombinedTargetInstrumentSetValidated = ...
                (isAnyTargetSet&&...
                isAnyInstrumentAvailable&&...
                isUnanticipatedInstrumentSetEffective);
            %% THROW EXCEPTION IF NECESSARY
            if ~isTargetSetValid || ...
                    ~isAnyTargetSet || ...
                    ~isAnyInstrumentAvailable || ...
                    ~isUnanticipatedInstrumentSetEffective   
                errId = ['MAPS:',mfilename('class'),':BadConditioning'];
                ValidationE = generate_MAPS_exception(errId);
                if ~isAnyTargetSet
                    errId = ['MAPS:',mfilename('class'),':NoTargets'];
                    ValidationE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        ValidationE,errId); 
                end
                if ~isAnyInstrumentAvailable
                    errId = ['MAPS:',mfilename('class'),':NoInstruments'];
                    ValidationE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        ValidationE,errId);
                end                
                if ~isTargetSetValid
                    ValidationE = addCause(ValidationE,TargetsE);
                end
                if ~isUnanticipatedInstrumentSetEffective
                    errId = ['MAPS:',...
                        mfilename('class'),':IneffectiveInstruments'];
                    ValidationE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        ValidationE,errId);
                end
                throw(ValidationE);
            end
        end
        %% SET METHOD FOR TARGETS
        function set.Targets(jConditioningObj,jTargetsObj)
            if ~isequal(jConditioningObj.pTargets,jTargetsObj)
                errId = ['MAPS:',mfilename('class'),':BadTargetsSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            jConditioningObj.pTargets = jTargetsObj;
        end
        %% SET METHOD FOR INSTRUMENTS
        function set.Instruments(jConditioningObj,jInstrumentsObj)
            if ~isequal(jConditioningObj.pInstruments,jInstrumentsObj)
                errId = [...
                    'MAPS:',mfilename('class'),':BadInstrumentsSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            jConditioningObj.pInstruments = jInstrumentsObj;
        end
        %% SET METHOD FOR OPTIONS
        function set.Options(jConditioningObj,iOptionsObj)
            if ~isequal(jConditioningObj.pOptions,iOptionsObj)
                errId = ['MAPS:',mfilename('class'),':BadOptionsSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            jConditioningObj.pOptions = iOptionsObj;
        end        
        %% GET METHOD FOR TARGETS
        function jTargetsObj = get.Targets(jConditioningObj)
            jTargetsObj = jConditioningObj.pTargets;
        end
        %% GET METHOD FOR INSTRUMENTS
        function jInstrumentsObj = get.Instruments(jConditioningObj)
            jInstrumentsObj = jConditioningObj.pInstruments;
        end
        %% GET METHOD FOR OPTIONS
        function jOptionsObj = get.Options(jConditioningObj)
            jOptionsObj = jConditioningObj.pOptions;
        end        
        %% GET METHOD FOR NON EMPTY FLAG
        function jConditioningIsNonEmpty = get.isNonEmpty(jConditioningObj)
            jConditioningIsNonEmpty = (...
                jConditioningObj.Targets.isNonEmpty||...
                jConditioningObj.Instruments.isNonEmpty);
        end
    end
    %% PRIVATE ACCESS METHODS
    methods (Access=private)    
        %% FUNCTION TO UPDATE VALIDATION FLAG IN CALLBACK
        function updateValidationFlag(jConditioningObj,~,~)
            % Callback function to handle changes in targets/instruments.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% RESET VALIDATION FLAG
            jConditioningObj...
                .isCombinedTargetInstrumentSetValidated = false;
            %% HANDLE CASE IN WHICH UPDATE HAS EMPTIED THE OBJECT
            if ~jConditioningObj.isNonEmpty
                jConditioningObj...
                    .isCombinedTargetInstrumentSetValidated = true;
            end
        end
    end
    %% PRIVATE ACCESS STATIC METHODS
    methods (Static=true,Access=private)
        function [iIndsCell,iValsCell] = createInversionCellArrays(...
                indsVec,horizon,matLogicals,valsmat)
            % Helper function to create inversion compatible cell arrays.
            %
            % INPUTS:
            %   -> indsVec: column vec of indices for the variables/shocks
            %   -> horizon: horizon up to which to create inversion cells
            %   -> matLogicals: matrix of logicals (either instruments or
            %      time period indicators for targets values data)
            %   -> valsMat: matrix of judgement values (for targets)
            %
            % OUTPUTS:
            %   -> iIndsCell: inversion indices cell array
            %   -> iValsCell: inversion values cell array
            %
            %% HANDLE ALTERNATIVE FUNCTION CALL CASES
            % If instrument inversion cell arrays are being set, there are
            % no values just indices (and time period logicals) in which
            % case only three inputs are passed into this function.
            if nargin < 4
                computeVals = false;
            else
                computeVals = true;
            end
            %% CREATE EMPTY INVERSION DATA ROW CELL ARRAYS
            iIndsCell = cell(1,horizon);
            if computeVals
                iValsCell = cell(1,horizon);
            end
            %% FILL INVERSION CELLS WITH INDICES & VALUES (IF APPLICABLE)
            % The logicals matrix is used to fill in the inversion indices
            % and values on a period-wise basis. See the Inversion package
            % and class contained within for more details.
            for t = 1:horizon
                iIndsCell{t} = indsVec(matLogicals(:,t));
                iIndsCell{t} = iIndsCell{t}(:);
                if computeVals
                    iValsCell{t} = valsmat(matLogicals(:,t),t);
                end
            end
        end
    end
end