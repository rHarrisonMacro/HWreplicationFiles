classdef Instructions < handle
    % This m-file defines an inversion instructions class.
    % This object manages all data associated with MAPS LSS model inversion
    % instructions.
    %
    % PUPLIC PROPERTIES:
    %   -> Targets: an instance of the targets inversion class
    %   -> Instruments: an instance of the instruments inversion class
    %       - Anticipated: an instance of the instruments data class
    %       - Unanticipated: another instance of the instruments data class
    %   -> Options: an instance of the inversion options class
    %   -> inversionType: "over-identified", "exactly-identified" or 
    %      "under-identified"
    %   -> inversionIncludesWeights: true/false
    %
    % EVENTS:
    %   -> none
    %
    % PULIC METHODS:
    %   -> constructor: requires a model & forecast shocks structure as
    %      inputs   
    %   -> invert: requires model solution matrices B, PHI & F as input, as
    %      well as projection initial conditions for the endogenous vars
    %   -> updateExistingShockValues: requires a new set of shocks as input
    %      (with optional update for either anticipated or unanticipated or
    %      both)
    %   -> validate: takes no input arguments
    %
    % DETAILS:
    %   -> This m-file defines an inversion instructions object for LSS 
    %      models.
    %   -> The Inversion.Instructions object is designed for in MAPS LSS 
    %      model inversion - see the "invert_LSS_model" function and the 
    %      "invert" method implemented below for details.  
    %   -> An Inversion.Instructions object can be instantiated with an LSS
    %      model and a set of forecast shocks (in a structure).
    %   -> Once this object has been instantiated it exposes the Targets, 
    %      Instruments & Options properties through "set" methods. These 
    %      objects in turn expose the means to set the inversion data for
    %      the targets and instruments - see those classes for details.
    %   -> Other than a constructor, this class also exposes public methods
    %      to implement an inversion, update the shock values in the object
    %      and validate that the inversion instructions meet the rules
    %      (i.e. define a correct and feasible set of instructions).
    %   -> One way of creating an inversion instructions object is to
    %      create a Judgements.Conditioning.Instructions object, which
    %      exposes a more sophisticated interface for setting the data, and
    %      then to use the "createInversionInstructions" method to convert
    %      the information to an inversion instructions object.
    %
    % NOTES:
    %   -> Note the use of private properties for the Targets, Instruments
    %      & Options objects. This is a useful technique for providing a
    %      controlled interface to users.
    %
    % This version: 25/11/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (Dependent=true)
        Targets
        Instruments
        Options
        inversionType
        inversionIncludesWeights
    end
    properties (SetAccess=private)
        nEndogVars
        nShocks
        horizon
    end
    properties (Access=private)
        pTargets
        pInstruments
        pOptions
        isValidated
    end
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR METHOD
        function iInstructionsObj = Instructions(Model,Shocks)
            % This is the constructor method for the Instructions class.
            %
            % INPUTS:
            %   -> Model: MAPS LSS model structure
            %   -> Shocks: structure containing at least one of:
            %       - anticipated: matrix of existing anticipated shocks
            %       - unanticipated: matrix of existing unanticipated 
            %         shocks
            %
            % OUTPUTS:
            %   -> iInstructionsObj: instance of the inversion instructions
            %      object
            %            
            %% CHECK INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadConstructNargin'];
                generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
            elseif ~isstruct(Model);
                errId = ['MAPS:',mfilename('class'),':BadConstructInput1'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~isstruct(Shocks);
                errId = ['MAPS:',mfilename('class'),':BadConstructInput2'];
                generate_and_throw_MAPS_exception(errId);
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
            %% EXAMINE CONTENT OF SHOCKS STRUCTURE
            [PHI,F] = unpack_model(Model,{'PHI','F'});
            try
                [ShocksInclude,H,nz] = ...
                    interrogate_forecast_shocks_structure(Shocks,PHI,F);
            catch ValidationE
                errId = ['MAPS:',mfilename('class'),':BadShocksStructure'];
                generate_MAPS_exception_add_cause_and_throw(...
                    ValidationE,errId);
            end
            %% SET PRIVATE ENOGENOUS VARIABE, SHOCK & FORECAST HORIZON DIMS
            nx = size(F,1);
            iInstructionsObj.nEndogVars = nx;
            iInstructionsObj.nShocks = nz;
            iInstructionsObj.horizon = H;
            %% INITIALISE TARGETS
            iInstructionsObj.pTargets = Inversion.Targets(nx,H);
            %% INITIALISE INSTRUMENTS
            % Unlike MAPS' LSS model forecast run execution functionality,
            % the inversion algorithm proceeds on the basis that both types
            % of shock exist (safe in the assumption that if they are set
            % to 0, then they cannot affect the inversion). Error handling
            % around setting information for shocks that do not exist is
            % handled in the instruments class (keying off the
            % "ShocksInclude" input).
            if ~ShocksInclude.anticipated
                Shocks.anticipated = zeros(nz,H);
            end
            if ~ShocksInclude.unanticipated;
                Shocks.unanticipated = zeros(nz,H);
            end
            iInstructionsObj.pInstruments = ...
                Inversion.Instruments(Shocks,ShocksInclude);
            %% INITIALISE OPTIONS
            iInstructionsObj.pOptions = Inversion.Options;
            %% ADD LISTENERS
            % Listen for changes in the state of the targets and
            % instruments objects (triggered by updates to their contents)
            % and update the validation flag accordingly.
            addlistener(iInstructionsObj.pInstruments,...
                'instrumentsUpdated',@(Src,Evnt) ...
                iInstructionsObj.updateValidationFlag(Src,Evnt));
            addlistener(iInstructionsObj.pTargets,...
                'targetsUpdated',@(Src,Evnt) ...
                iInstructionsObj.updateValidationFlag(Src,Evnt));
            %% SET VALIDATION FLAG
            iInstructionsObj.isValidated = false;
        end
        %% INVERT METHOD
        function NewShocks = invert(iInstructionsObj,B,PHI,F,xT)
            % This method executes an LSS model inversion.
            %
            % INPUTS:
            %   -> B: backward-loadings in model solution
            %   -> PHI: loadings on shocks in model solution
            %   -> F: loadings on future shocks in model solution
            %   -> xT: vector of initial conditions for the endogenous vars
            %
            % OUTPUTS:
            %   -> NewShocks: structure containing the following:
            %       - anticipated: matrix of new anticipated shock values
            %       - unanticipated: matrix of new unanticipated shock
            %         values
            %
            %% CHECK NUMBER OF INPUTS
            if nargin < 5
                errId = ['MAPS:',mfilename('class'),':BadInvertNargin'];
                generate_and_throw_MAPS_exception(...
                    errId,{num2str(nargin-1)});
            end
            %% VALIDATE INVERSION INSTRUCTIONS
            iInstructionsObj.validate();
            %% CHECK SOLUTION MATRIX INPUTS ARE COMPATIBLE WITH EACH OTHER
            validate_LSS_model_solution_matrices(B,PHI,F);
            %% CHECK ENOGENOUS VARIBALE INITIAL CONDITIONS
            % They must be compatible in dimension with the solution matrix
            % inputs and with the dimensions in construction of this
            % object.
            nx = interrogate_LSS_model_variable_initial_conditions(xT,B);
            if nx ~= iInstructionsObj.nEndogVars
                errId = [...
                    'MAPS:',mfilename('class'),':BadInvertEndogVarCount'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CHECK SHOCK DIMENSIONS ARE COMPATIBLE WITH THIS OBJECT
            nz = size(PHI,2);
            if nz ~= iInstructionsObj.nShocks
                errId = [...
                    'MAPS:',mfilename('class'),':BadInvertShockCount'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CHECK COKPATIBILITY OF F MATRIX WITH OBJECT
            if ~any(any(F)) && ~iInstructionsObj...
                    .pInstruments.Anticipated.isLockedForEditing
                errId = [...
                    'MAPS:',mfilename('class'),':BadAnticipatedInvert'];
                generate_and_throw_MAPS_exception(errId);            
            end
            %% CALL INVERT LSS MODEL FUNCTION
            NewShocks = invert_LSS_model_new(iInstructionsObj,B,PHI,F,xT);
        end
        %% UPDATE EXISTING SHOCK VALUES METHOD
        function updateExistingShockValues(iInstructionsObj,UpdatedShocks)
            % This method updates the shock values in the inversion.
            % This facilitates repeated use of the same object in different
            % settings.
            %
            % INPUTS:
            %   -> UpdatedShocks: structure containing either of the 
            %      following:
            %       - anticipated: matrix of new anticipated shock values
            %       - unanticipated: matrix of new unanticipated shock
            %         values
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK NUMBER OF INPUTS
            if nargin < 2
                errId = [...
                    'MAPS:',mfilename('class'),':BadUpdateShocksNargin'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CHECK STRUCTURE & THEN ATTEMPT UPDATE
            % Update is implemented in a method on the Instruments class.
            try
                check_field_names_in_structure(UpdatedShocks,...
                    'Structure to update existing shock values',...
                    '',{'anticipated';'unanticipated'});
                iInstructionsObj.Instruments.updateExistingShockValues(...
                    UpdatedShocks);
            catch UpdateE
                errId = [...
                    'MAPS:',mfilename('class'),':BadUpdateShocksInput1'];
                generate_MAPS_exception_add_cause_and_throw(UpdateE,errId);
            end
        end
        %% VALIDATE FUNCTION
        function validate(iInstructionsObj)
            % This method validates the inversion instructions object.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% EXIT IF ALREADY VALIDATED
            if iInstructionsObj.isValidated
                return
            end
            %% VALIDATE TARGETS & INSTRUMENTS DATA
            % Note that if either of these fail validation, then the rest
            % of the validation is not executed (on the grounds that there
            % is no point in validating against the rules if the underlying
            % data is wrong).
            isTargetSetValid = true;
            isInstrumentSetValid = true;
            try
                iInstructionsObj.Targets.validate();
            catch TargetDataE
                isTargetSetValid = false;
            end
            try
                iInstructionsObj.Instruments.validate();
            catch InstrumentDataE
                isInstrumentSetValid = false;
            end
            if ~isTargetSetValid || ~isInstrumentSetValid
                errId = [...
                    'MAPS:',mfilename('class'),':InvalidInversionData'];
                InversionDataE = generate_MAPS_exception(errId);
                if ~isTargetSetValid
                    InversionDataE = addCause(InversionDataE,TargetDataE);
                end
                if ~isInstrumentSetValid
                    InversionDataE = addCause(...
                        InversionDataE,InstrumentDataE);
                end
                throw(InversionDataE);
            end
            %% SET FLAGS FOR EACH OF THE INVERSION RULES
            isInversionInstructionsNonEmpty = true;
            isTargetSetImplementable = true;
            isInstrumentSetIdentifiable = true;
            isUnanticipatedInstrumentSetIdentifiable = true;
            isTargetWeightUseValid = true;
            isInstrumentWeightUseValid = true;
            %% INVERSION INSTRUCTIONS MUST CONTAIN SOMETHING
            if iInstructionsObj.Targets.totalNumber==0 && ...
                    iInstructionsObj.Instruments.totalNumber==0
                isInversionInstructionsNonEmpty = false;
            end
            %% TARGET SET MUST BE IMPLEMENTABLE
            if iInstructionsObj.Targets.totalNumber>0 && ...
                    iInstructionsObj.Instruments.totalNumber==0
                isTargetSetImplementable = false;
            end
            %% INSTRUMENT SET MUST BE IDENTIFIABLE
            if iInstructionsObj.Instruments.totalNumber>0 && ...
                    iInstructionsObj.Targets.totalNumber==0
                isInstrumentSetIdentifiable = false;
            end
            %% UNANTICIPATED SHOCKS MUST HAVE HORIZON <= TARGETS
            if iInstructionsObj.Instruments.Unanticipated.horizon > ...
                    iInstructionsObj.Targets.horizon
                isUnanticipatedInstrumentSetIdentifiable = false;
            end
            %% TARGET WEIGHTS CAN ONLY EXIST IN UNDER-IDENTIFIED CASES
            switch iInstructionsObj.inversionType
                case {'over-identified','exactly-identified'}
                    if iInstructionsObj.Targets.isWeightsNonEmpty
                        isTargetWeightUseValid = false;
                    end
            end
            %% INSTRUMENT WEIGHTS CAN ONLY EXIST IN OVER-IDENTIFIED CASES
            switch iInstructionsObj.inversionType
                case {'under-identified','exactly-identified'}
                    if iInstructionsObj.Instruments.isWeightsNonEmpty
                        isInstrumentWeightUseValid = false;
                    end
            end
            %% THROW EXEPTION DETAILING VALIDATION ERRORS
            if ~isInversionInstructionsNonEmpty || ...
                    ~isTargetSetImplementable || ...
                    ~isInstrumentSetIdentifiable || ...
                    ~isUnanticipatedInstrumentSetIdentifiable || ...
                    ~isTargetWeightUseValid || ...
                    ~isInstrumentWeightUseValid
                errId = [...
                    'MAPS:',mfilename('class'),':InvalidInstructions'];
                InstructionsE = generate_MAPS_exception(errId);
                if ~isInversionInstructionsNonEmpty
                    errId = ['MAPS:',mfilename('class'),...
                        ':NoInversionInstructions'];
                    InstructionsE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        InstructionsE,errId);
                end
                if ~isTargetSetImplementable
                    errId = ['MAPS:',mfilename('class'),...
                        ':TargetsNotImplementable'];
                    InstructionsE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        InstructionsE,errId);
                end
                if ~isInstrumentSetIdentifiable
                    errId = ['MAPS:',mfilename('class'),...
                        ':InstrumentsNotIdentifiable'];
                    InstructionsE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        InstructionsE,errId);
                end
                if ~isUnanticipatedInstrumentSetIdentifiable
                    errId = ['MAPS:',mfilename('class'),...
                        ':UnanticipatedInstrumentsNotIdentifiable'];
                    InstructionsE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        InstructionsE,errId);
                end
                if ~isTargetWeightUseValid
                    errId = ['MAPS:',mfilename('class'),...
                        ':InvalidTargetWeightUse'];
                    InstructionsE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        InstructionsE,errId);
                end
                if ~isInstrumentWeightUseValid
                    errId = ['MAPS:',mfilename('class'),...
                        ':InvalidInstrumentWeightUse'];
                    InstructionsE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        InstructionsE,errId);
                end
                throw(InstructionsE);
            end
            %% UPDATE VALIDATION FLAG
            iInstructionsObj.isValidated = true;
        end
        %% SET METHOD FOR TARGETS
        function set.Targets(iInstructionsObj,iTargetsObj)
            if ~isequal(iInstructionsObj.pTargets,iTargetsObj)
                errId = ['MAPS:',mfilename('class'),':BadTargetsSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            iInstructionsObj.pTargets = iTargetsObj;
        end
        %% SET METHOD FOR INSTRUMENTS
        function set.Instruments(iInstructionsObj,iInstrumentsObj)
            if ~isequal(iInstructionsObj.pInstruments,iInstrumentsObj)
                errId = [...
                    'MAPS:',mfilename('class'),':BadInstrumentsSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            iInstructionsObj.pInstruments = iInstrumentsObj;
        end
        %% SET METHOD FOR OPTIONS
        function set.Options(iInstructionsObj,iOptionsObj)
            if ~isequal(iInstructionsObj.pOptions,iOptionsObj)
                errId = ['MAPS:',mfilename('class'),':BadOptionsSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            iInstructionsObj.pOptions = iOptionsObj;
        end
        %% GET METHOD FOR TARGETS
        function iTargetsObj = get.Targets(iInstructionsObj)
            iTargetsObj = iInstructionsObj.pTargets;
        end
        %% GET METHOD FOR INSTRUMENTS
        function iInstrumentsObj = get.Instruments(iInstructionsObj)
            iInstrumentsObj = iInstructionsObj.pInstruments;
        end
        %% GET METHOD FOR OPTIONS
        function iOptionsObj = get.Options(iInstructionsObj)
            iOptionsObj = iInstructionsObj.pOptions;
        end
        %% GET METHOD FOR INVERSION TYPE
        function inversionType = get.inversionType(iInstructionsObj)
            if iInstructionsObj.Instruments.totalNumber > ...
                    iInstructionsObj.Targets.totalNumber
                inversionType = 'over-identified';
            elseif iInstructionsObj.Instruments.totalNumber < ...
                    iInstructionsObj.Targets.totalNumber
                inversionType = 'under-identified';
            else
                inversionType = 'exactly-identified';
            end
        end
        %% GET METHOD FOR WEIGHTS FLAG
        function inversionWeightsFlag = ...
                get.inversionIncludesWeights(iInstructionsObj)
            if iInstructionsObj.Targets.isWeightsNonEmpty || ...
                    iInstructionsObj.Instruments.isWeightsNonEmpty
                inversionWeightsFlag = true;
            else
                inversionWeightsFlag = false;
            end
        end
    end
    %% PRIVATE ACCESS METHODS
    methods (Access=private)
        function updateValidationFlag(iInstructionsObj,~,~)
            % This callback function resets the validation flag to false.
            % It is triggered in the event that any of the underlying
            % inversion data is updated. See the constructor for details of
            % how/where the listeners are set.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% SET VALIDATION FLAG TO FALSE
            % This ensures the inversion object is re-validated - see the
            % "validate" method for details.
            iInstructionsObj.isValidated = false;
        end
    end
end