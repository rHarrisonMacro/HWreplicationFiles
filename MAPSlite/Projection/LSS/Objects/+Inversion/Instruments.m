classdef Instruments < handle
    % This m-file defines the instruments part of an inversion object.
    % It encapsulates all of the logic around management of the anticipated
    % and unanticipated instrument data. It cannot be instantiated outside
    % of the Inversion.Instructions class.
    %
    % PUBLIC PROPERTIES:
    %   -> Anticipated: object of the instruments data class
    %   -> Unanticipated: another object of the instruments data class
    %   -> includesAnticipatedInstruments: true/false
    %   -> includesUnanticipatedInstruments: true/false
    %   -> totalNumber: non-negative numeric integer
    %   -> isWeightsNonEmpty: true/false
    %
    % EVENTS:
    %   -> instrumentsUpdated: broadcast if the state of the object changes
    %
    % PUBLIC METHODS:
    %   -> none
    %
    % DETAILS:
    %   -> This m-file defines an instruments object as part of an
    %      Inversion.Instructions object. It cannot be instantiated outside
    %      of that object and has no public methods other than "get" and 
    %      "set" methods.
    %   -> It manages the cross-validation of the anticipated &
    %      unanticipated instruments. In particular, it is not valid to
    %      supply weights for one of the instrument types and not the
    %      other.
    %   -> Other than this validation, the main purpose this class serves
    %      is as a holder for instrument data, facilitating the setting and
    %      getting of this information as part of the overall instructions
    %      object.
    %
    % NOTES:
    %   -> None. 
    %
    % This version: 25/11/2013
    % Author(s): Matt Waldron

    %% PROPERTIES
    properties (Dependent=true)
        Anticipated
        Unanticipated
        includesAnticipatedInstruments
        includesUnanticipatedInstruments
        totalNumber
        isWeightsNonEmpty
    end
    properties (SetAccess=private)
        allowAnticipatedInstruments
        allowUnanticipatedInstruments
    end
    properties (Access=private)
        pAnticipated
        pUnanticipated
        isValidated
    end
    %% EVENTS
    events
        instrumentsUpdated
    end
    %% PUBLIC METHODS
    methods
        %% SET METHOD FOR ANTICIPATED INSTRUMENTS
        function set.Anticipated(iInstrumentsObj,iInstrumentsDataObj)
            if ~isequal(...
                    iInstrumentsObj.pAnticipated,iInstrumentsDataObj)
                errId = [...
                    'MAPS:',mfilename('class'),':BadAnticipatedSet'];
                generate_and_throw_MAPS_exception(errId);    
            end
            iInstrumentsObj.pAnticipated = iInstrumentsDataObj;
        end
        %% SET METHOD FOR UNANTICIPATED INSTRUMENTS
        function set.Unanticipated(iInstrumentsObj,iInstrumentsDataObj)
            if ~isequal(...
                    iInstrumentsObj.pUnanticipated,iInstrumentsDataObj)
                errId = [...
                    'MAPS:',mfilename('class'),':BadUnanticipatedSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            iInstrumentsObj.pUnanticipated = iInstrumentsDataObj;
        end
        %% GET METHOD FOR UNANTICIPATED INSTRUMENTS
        function uiObj = get.Unanticipated(iInstrumentsObj)
            uiObj = iInstrumentsObj.pUnanticipated;
        end
        %% GET METHOD FOR ANTICIPATED INSTRUMENTS
        function aiObj = get.Anticipated(iInstrumentsObj)         
            aiObj = iInstrumentsObj.pAnticipated;
        end
        %% GET METHOD FOR ANTICIPATED SHOCK JUDGEMENT FLAG
        function aiIsNonEmpty = ...
                get.includesAnticipatedInstruments(iInstrumentsObj)
            aiIsNonEmpty = (iInstrumentsObj.Anticipated.totalNumber>0);
        end
        %% GET METHOD FOR UNANTICIPATED SHOCK JUDGEMENT FLAG
        function uiIsNonEmpty = ...
                get.includesUnanticipatedInstruments(iInstrumentsObj)
            uiIsNonEmpty = (iInstrumentsObj.Unanticipated.totalNumber>0);
        end   
        %% GET METHOD FOR TOTAL NUMBER OF INSTRUMENTS
        function nInstruments = get.totalNumber(iInstrumentsObj)
           nInstruments = iInstrumentsObj.Anticipated.totalNumber+...
               iInstrumentsObj.Unanticipated.totalNumber;
        end
        %% GET METHOD FOR WEIGHTS
        function isWeightsNonEmpty = get.isWeightsNonEmpty(iInstrumentsObj)
            isWeightsNonEmpty = (...
                iInstrumentsObj.Anticipated.isWeightsNonEmpty||...
                iInstrumentsObj.Unanticipated.isWeightsNonEmpty);
        end
    end
    %% PRIVATE METHODS TO INVERSION.INSTRUCTIONS OBJECTS
    methods (Access={?Inversion.Instructions})
        %% CONSTRUCTOR
        function iInstrumentsObj = Instruments(Shocks,ShocksInclude)
            % Constructor method for this class.
            %
            % INPUTS:
            %   -> Shocks: structure of existing shock values
            %   -> ShocksInclude: structure describing which instruments 
            %      can be actively set
            %
            % OUTPUTS:
            %   -> iInstrumentsObj: instance of the object
            %
            %% EXTRACT SHOCKS FLAGS & SET PROPERTIES ACCORDINGLY
            aiPermitted = ShocksInclude.anticipated;
            uiPermitted = ShocksInclude.unanticipated;
            iInstrumentsObj.allowAnticipatedInstruments = aiPermitted;
            iInstrumentsObj.allowUnanticipatedInstruments = uiPermitted;
            %% INITIALISE ANTICIPATED INSTRUMENTS OBJECT
            % Note that a listener is set for changes in the state of
            % the anticipated instruments object. This facilitates
            % "efficient" validation, whereby the object is only
            % re-validated when it is updated.
            iInstrumentsObj.pAnticipated = Inversion.InstrumentsData(...
                Shocks.anticipated,aiPermitted);
            addlistener(iInstrumentsObj.pAnticipated,...
                'instrumentsDataUpdated',...
                @(Src,Evnt) iInstrumentsObj...
                .updateFlagAndNotifyForValidation(Src,Evnt));
            %% INITIALISE UNANTICIPATED INSTRUMENTS OBJECT
            % See the comment above re listeners.
            iInstrumentsObj.pUnanticipated = Inversion.InstrumentsData(...
                Shocks.unanticipated,uiPermitted);
            addlistener(iInstrumentsObj.pUnanticipated,...
                'instrumentsDataUpdated',...
                @(Src,Evnt) iInstrumentsObj...
                .updateFlagAndNotifyForValidation(Src,Evnt));
        end
        %% UPDATE EXISTING SHOCK VALUES
        function updateExistingShockValues(iInstrumentsObj,UpdatedShocks)
            % Method to update existing shock values.
            %
            % INPUTS:
            %   -> UpdatedShocks: structure of new shock values for either
            %      anticipated shocks, unanticipated shocks or both
            %
            % OUTPUTS:
            %   -> none
            %
            %% UPDATE ANTICIPATED SHOCKS (IF INCLUDED & IF PERMITTED)
            if isfield(UpdatedShocks,'anticipated')
                if ~iInstrumentsObj.allowAnticipatedInstruments
                    errId = ['MAPS:',...
                        mfilename('class'),':BadAnticipatedUpdate'];
                    generate_and_throw_MAPS_exception(errId);
                end
                iInstrumentsObj.Anticipated.updateExistingShockValues(...
                    UpdatedShocks.anticipated);
            end
            %% UPDATE UNANTICIPATED SHOCKS (IF INCLUDED & IF PERMITTED)
            if isfield(UpdatedShocks,'unanticipated')
                if ~iInstrumentsObj.allowUnanticipatedInstruments
                    errId = ['MAPS:',...
                        mfilename('class'),':BadUnanticipatedUpdate'];
                    generate_and_throw_MAPS_exception(errId);
                end
                iInstrumentsObj.Unanticipated.updateExistingShockValues(...
                    UpdatedShocks.unanticipated);
            end
        end
        %% VALIDATION METHOD
        function validate(iInstrumentsObj)
            % Method to validate the inversion instruments.
            % It contains the logic for validation of the combined set of
            % instruments.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% EXIT IF ALREADY VALIDATED
            if iInstrumentsObj.isValidated
                return
            end
            %% SET VALIDATION FLAGS
            isAnticipatedSetValid = true;
            isUnanticipatedSetValid = true;
            isWeightUseComplete = true;
            %% VALIDATE ANTICIPATED INSTRUMENTS
            try
                iInstrumentsObj.Anticipated.validate();
            catch AnticipatedDataE
                isAnticipatedSetValid = false;
            end
            %% VALIDATE UNANTICIPATED INSTRUMENTS
            try
                iInstrumentsObj.Unanticipated.validate();
            catch UnanticipatedDataE
                isUnanticipatedSetValid = false;
            end
            %% VALIDATE WEIGHTS ARE USED CORRECTLY
            % If weights are set at all, then they must be set for both 
            % instrument types;
            if iInstrumentsObj.includesAnticipatedInstruments && ...
                    iInstrumentsObj.includesUnanticipatedInstruments
                if iInstrumentsObj.Anticipated.isWeightsNonEmpty ~= ...
                    iInstrumentsObj.Unanticipated.isWeightsNonEmpty
                    isWeightUseComplete = false;
                end
            end 
            %% THROW EXCEPTION (IF APPLICABLE)
            if ~isAnticipatedSetValid || ...
                    ~isUnanticipatedSetValid || ...
                    ~isWeightUseComplete
                errId = [...
                    'MAPS:',mfilename('class'),':InvalidInstruments'];
                InstrumentsDataE = generate_MAPS_exception(errId);
                if ~isAnticipatedSetValid
                    InstrumentsDataE = addCause(...
                        InstrumentsDataE,AnticipatedDataE);
                end
                if ~isUnanticipatedSetValid
                    InstrumentsDataE = addCause(...
                        InstrumentsDataE,UnanticipatedDataE);
                end
                if ~isWeightUseComplete
                    errId = [...
                        'MAPS:',mfilename('class'),':IncompleteWeightUse'];
                    InstrumentsDataE = ...
                        generate_MAPS_exception_and_add_as_cause(...
                        InstrumentsDataE,errId);
                end
                throw(InstrumentsDataE);
            end
            %% RESET VALIDATION FLAG IF NO EXCEPTIONS WERE FOUND
            iInstrumentsObj.isValidated = true;
        end
    end
    %% PRIVATE ACCESS METHODS
    methods (Access=private)
        %% FUNCTION TO UPDATE VALIDATION FLAG IN CALLBACK TO UPDATE EVENTS
        function updateFlagAndNotifyForValidation(iInstrumentsObj,~,~)
            % Callback function to handle changes in state of data objects.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% UPDATE VALIDATION FLAG
            iInstrumentsObj.isValidated = false;
            %% BROADCAST STATE CHANGE EVENT
            notify(iInstrumentsObj,'instrumentsUpdated');
        end
    end
end