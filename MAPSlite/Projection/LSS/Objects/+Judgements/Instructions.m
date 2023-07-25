classdef Instructions < handle
    % This m-file defines a judgements instructions object.
    % This object allows for all LSS model judgements to be set: judgements
    % directly to shocks, judgements to endogenous variables as part of
    % conditioning and judgements to time-varying trends.
    %
    % PUBLIC PROPERTIES:
    %   -> Conditioning: conditioning/inversion instructions object
    %   -> Shocks: shock judgements object
    %   -> TimeVaryingTrends: time-varying trend judgements objects
    %   -> includesConditioningJudgements: true/false
    %   -> includesShockJudgements: true/false
    %   -> includesTimeVaryingTrendJudgements: true/false
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> constructor: requires a model & forecast run data structure as
    %      inputs
    %   -> validate: validates the whole object, taking a model & forecast
    %      run dataset as input
    %
    % DETAILS:
    %   -> This m-file defines a judgement instructions object for LSS 
    %      models. This is the "Forecast" part of an LSS model provisional
    %      run dataset - see MAPS' forecast run execution functions for
    %      more details.
    %   -> The Judgements.Instructions object is designed for use as part
    %      of MAPS' LSS model impose judgement functionality - see
    %      impose_judgement_using_LSS_model.    
    %   -> A Judgements.Instructions object can be instantiated with an LSS
    %      model and forecast run dataset input.
    %   -> Once this object has been instantiated it exposes the Shocks, 
    %      Conditioning & TimeVaryingTrend properties through "set" 
    %      methods. These objects in turn expose a number of different 
    %      methods for setting the judgement conditioning data. See the
    %      Judgements.Shocks, Judgements.Conditioning.Instructions, 
    %      Judgements.Shocks & the Judgements.Data classes for more
    %      details.
    %   -> Other than a constructor, the only public method exposed by this
    %      class is a "validate" method which requires a model and forecast
    %      run structure as input and which will validate the entire
    %      instructions object.
    %   -> This class takes care of cross-validation of judgements made 
    %      directly to shocks and judgements made indirectly via shock
    %      instruments in an inversion (which cannot conflict with each 
    %      other). Validation of the conditioning judgements object and its
    %      state is taken care of by the conditioning instructions objects
    %      own validation method.
    %
    % NOTES:
    %   -> Note the use of private properties for the Conditioning, Shocks
    %      & TimeVaryingTrend objects. This is a useful technique for
    %      providing a controlled interface to users.
    %
    % This version: 09/12/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (Dependent=true)
        Conditioning
        Shocks
        TimeVaryingTrends
        includesConditioningJudgements
        includesShockJudgements
        includesTimeVaryingTrendJudgements
    end
    properties (SetAccess=private)
        modelHasTT
    end
    properties (Access=private)
        pConditioning
        pShocks
        pTimeVaryingTrends
        isAnticipatedShocksOverlapValidated
        isUnanticipatedShocksOverlapValidated
    end    
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR
        function jInstructionsObj = Instructions(Model,RunData)
            % This is the constructor method for the Instructions class.
            %
            % INPUTS:
            %   -> Model: MAPS LSS model structure
            %   -> RunData: forecast run structure
            %
            % OUTPUTS:
            %   -> jInstructionsObj: instance of the conditioning object
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
            end
            %% CHECK MODEL IS LSS
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
            %% INITIALISE CONDITIONING JUDGEMENTS
            jInstructionsObj.pConditioning = ...
                Judgements.Conditioning.Instructions(Model,RunData);   
            %% INITIALISE SHOCK JUDGEMENTS
            jInstructionsObj.pShocks = Judgements.Shocks(Model,H);
            %% INITIALISE TIME-VARYING TREND JUDGEMENTS
            modelHasTimeVaryingTrends = unpack_model(...
                Model,'modelHasTimeVaryingTrends');
            jInstructionsObj.modelHasTT = modelHasTimeVaryingTrends;
            jInstructionsObj.pTimeVaryingTrends = ...
                Judgements.TimeVaryingTrends(Model,H);
            %% SET FLAGS FOR SHOCK OVERLAPS VALIDATION
            % A judgement instructions object is invalid if the
            % instructions includes the same shock under the same
            % assumption (anticipated or unanticipated) as both an
            % instrument in conditioining and as a direct judgement to that
            % shock (since this instruction is conflicting).
            jInstructionsObj.isAnticipatedShocksOverlapValidated = true;
            jInstructionsObj.isUnanticipatedShocksOverlapValidated = true;
            %% ADD LISTENERS FOR CHANGES IN STATE OF INSTRUMENTS/JUDGEMENTS
            % Listen for changes in state of each of the shock instruments
            % and judgements. If their state does change, a callback
            % function is executed which updates the relevant validation
            % flag. This is designed so that validation does not need to be
            % repeated in the event that the state of the objects does not
            % change.
            addlistener(jInstructionsObj.pConditioning.Instruments,...
                'jaInstrumentsObjStateChanged',@(Src,Evnt) ...
                jInstructionsObj.updateValidationFlag(...
                Src,Evnt,'Anticipated'));
            addlistener(jInstructionsObj.pConditioning.Instruments,...
                'juInstrumentsObjStateChanged',@(Src,Evnt) ...
                jInstructionsObj.updateValidationFlag(...
                Src,Evnt,'Unanticipated'));
            addlistener(jInstructionsObj.pShocks,...
                'jaShocksObjStateChanged',@(Src,Evnt) ...
                jInstructionsObj.updateValidationFlag(...
                Src,Evnt,'Anticipated'));
            addlistener(jInstructionsObj.pShocks,...
                'juShocksObjStateChanged',@(Src,Evnt) ...
                jInstructionsObj.updateValidationFlag(...
                Src,Evnt,'Unanticipated'));            
        end
        %% VALIDATION FUNCTION
        function validate(jInstructionsObj,Model,RunData)
            % This method validates the entire instructions object.
            % It will throw an exception in the event that any of the MAPS
            % LSS model impose judgement rules is broken.
            %
            % INPUTS:
            %   -> Model: MAPS LSS model structure
            %   -> RunData: forecast run dataset structure
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK NUMBER OF INPUTS
            % Note that the inputs themselves are validated (to the extent
            % that they can be) in the conditioining object validation
            % function.
            if nargin < 3
                errId = ['MAPS:',mfilename('class'),':BadValidateNargin'];
                generate_and_throw_MAPS_exception(...
                    errId,{num2str(nargin-1)});
            end
            %% SET VALIDATION FLAGS
            isConditioningValid = true;
            isAnticipatedShocksOverlapValid = true;
            isUnanticipatedShocksOverlapValid = true;
            %% VALIDATE CONDITIONING JUDGEMENTS 
            % See the content of Judgements.Conditioning.validate for
            % details.
            try
                jInstructionsObj.Conditioning.validate(Model,RunData);
            catch ConditioningValidationE
                isConditioningValid = false;
            end   
            %% VALIDATE ANTICIPATED SHOCK CONFLICTS (IF NECESSARY)
            % Note that if no state change events to either anticipated
            % shock judgements or anticipated conditioning instruments 
            % occurred since the previous validation, then the validation
            % flag will be true and the validation will not be executed. If
            % that is not the case, then a private helper function is
            % called.
            if ~jInstructionsObj.isAnticipatedShocksOverlapValidated
                try
                    jInstructionsObj...
                        .validateShockJudgementAndUsageIsNonOverlapping(...
                        'Anticipated');
                catch AnticipatedOverlapsE
                    isAnticipatedShocksOverlapValid = false;
                end
            end
            jInstructionsObj.isAnticipatedShocksOverlapValidated = ...
                isAnticipatedShocksOverlapValid;
            %% VALIDATE UNANTICIPATED SHOCK CONFLICTS (IF NECESSARY)
            % See the cell above for some comments.
            if ~jInstructionsObj.isUnanticipatedShocksOverlapValidated
                try
                    jInstructionsObj...
                        .validateShockJudgementAndUsageIsNonOverlapping(...
                        'Unanticipated');
                catch UnanticipatedOverlapsE
                    isUnanticipatedShocksOverlapValid = false;
                end
            end
            jInstructionsObj.isUnanticipatedShocksOverlapValidated = ...
                isUnanticipatedShocksOverlapValid;
            %% THROW EXCEPTION AS NECESSARY
            if ~isConditioningValid || ...
                    ~isAnticipatedShocksOverlapValid || ...
                    ~isUnanticipatedShocksOverlapValid
                errId = ['MAPS:',...
                    mfilename('class'),':BadJudgementInstructions'];
                JudgementInstructionsE = generate_MAPS_exception(errId);
                if ~isConditioningValid
                    JudgementInstructionsE = addCause(...
                        JudgementInstructionsE,ConditioningValidationE);
                end
                if ~isAnticipatedShocksOverlapValid
                    JudgementInstructionsE = addCause(...
                        JudgementInstructionsE,AnticipatedOverlapsE);
                end
                if ~isUnanticipatedShocksOverlapValid
                    JudgementInstructionsE = addCause(...
                        JudgementInstructionsE,UnanticipatedOverlapsE);
                end
                throw(JudgementInstructionsE);
            end
        end
        %% SET METHOD FOR CONDITIONING JUDGEMENTS
        function set.Conditioning(jInstructionsObj,jConditioningObj)
            if ~isequal(jInstructionsObj.pConditioning,jConditioningObj)
                errId = [...
                    'MAPS:',mfilename('class'),':BadConditioningSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            jInstructionsObj.pConditioning = jConditioningObj;
        end
        %% SET METHOD FOR SHOCK JUDGEMENTS
        function set.Shocks(jInstructionsObj,jShocksObj)
            if ~isequal(jInstructionsObj.pShocks,jShocksObj)
                errId = ['MAPS:',mfilename('class'),':BadShocksSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            jInstructionsObj.pShocks = jShocksObj;
        end    
        %% SET METHOD FOR TIME VARYING TREND JUDGEMENTS
        function set.TimeVaryingTrends(jInstructionsObj,jCell)
            if ~jInstructionsObj.modelHasTT
                errId = [...
                    'MAPS:',mfilename('class'),':BadTimeVaryingTrendSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            try
                jInstructionsObj.pTimeVaryingTrends.overwrite(jCell);
            catch InputValidationE
                errId = ['MAPS:',...
                    mfilename('class'),':BadTimeVaryingTrendInput'];
                generate_MAPS_exception_add_cause_and_throw(...
                    InputValidationE,errId);
            end
        end
        %% GET METHOD FOR CONDITIONING
        function jConditioningObj = get.Conditioning(jInstructionsObj)
            jConditioningObj = jInstructionsObj.pConditioning;
        end
        %% GET METHOD FOR SHOCKS
        function jShocksObj = get.Shocks(jInstructionsObj)
            jShocksObj = jInstructionsObj.pShocks;
        end 
        %% GET METHOD FOR TIME VARYING TRENDS
        function jttObj = get.TimeVaryingTrends(jInstructionsObj)
            jttObj = jInstructionsObj.pTimeVaryingTrends;
        end        
        %% GET METHOD FOR CONDITIONING JUDGEMENTS FLAG
        function jConditioningIsNonEmpty = ...
                get.includesConditioningJudgements(jInstructionsObj)
           jConditioningIsNonEmpty = ...
               jInstructionsObj.Conditioning.isNonEmpty;  
        end
        %% GET METHOD FOR SHOCKS JUDGEMENTS FLAG
        function jShocksIsNonEmpty = ...
                get.includesShockJudgements(jInstructionsObj)
           jShocksIsNonEmpty = jInstructionsObj.Shocks.isNonEmpty;  
        end
        %% GET METHOD FOR TIME VARYING TRENDS JUDGEMENTS FLAG
        function jttIsNonEmpty = ...
                get.includesTimeVaryingTrendJudgements(jInstructionsObj)
           jttIsNonEmpty = (...
               jInstructionsObj.modelHasTT&&...
               jInstructionsObj.TimeVaryingTrends.isNonEmpty);  
        end 
    end
    %% PRIVATE ACCESS METHODS
    methods (Access=private)
        function validateShockJudgementAndUsageIsNonOverlapping(...
                jInstructionsObj,typeStr)
            % This helper validates shock judgements/instruments.
            % It will throw an exception in the event that the shock
            % judgements and conditioning instruments overlap (conflict).
            %
            % INPUTS:
            %   -> typeStr: either "Anticipated" or "Unanticipated"
            %
            % OUTPUTS:
            %   -> none
            %
            %% CONCATENATE THE JUDGEMENT & INSTRUMENT MNEMONICS TOGETHER
            mergedMnems = [...
                jInstructionsObj.Conditioning.Instruments.(typeStr).mnems;
                jInstructionsObj.Shocks.(typeStr).mnems];
            %% CONCATENATE MATRICES OF LOGICALS (TIME PERIOD INDICATORS)
            mergedLogicals = [...
                jInstructionsObj.Conditioning.Instruments.(typeStr).mat;
                jInstructionsObj.Shocks.(typeStr).matLogicals];
            %% USE ABSTRCAT METHOD ON DATA BASE CLASS TO DO VALIDATION
            errId = [...
                'MAPS:',mfilename('class'),':',typeStr,'ShocksOverlap'];
            Judgements.Data.BaseClass.validateLogicalsAreNonOverlapping(...
                mergedMnems,mergedLogicals,errId);
        end
        %% FUNCTION TO UPDATE VALIDATION FLAG IN CALLBACK TO UPDATE EVENTS
        function updateValidationFlag(jInstructionsObj,~,~,typeStr)
            % This helper updates the validtaion flag as callback.
            % It is a callback function in response to changes in state of
            % the shock judgements or conditioing instruments. See the
            % constructor in this class for details of how the listeners
            % are set.
            %
            % INPUTS:
            %   -> typeStr: either "Anticipated" or "Unanticipated"
            %
            % OUTPUTS:
            %   -> none
            %    
            %% SET FLAG AS INSTRUCTED
            % Setting to false means that the "validate" method will
            % re-check that the shock judgements and instruments are
            % non-overlapping/conflicting.
            jInstructionsObj...
                .(['is',typeStr,'ShocksOverlapValidated']) = false;
        end
    end
end