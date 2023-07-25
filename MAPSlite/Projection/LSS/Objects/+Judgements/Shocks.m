classdef Shocks < handle
    % This m-file defines an LSS model shock judgements object.
    % This object forms part of a MAPS LSS model judgement instructions 
    % object as encapsulated by the classes that form part of the 
    % Judgements package. It is not possible to instantiate this class
    % outside of the Judgements.Instructions class.
    %
    % PUBLIC PROPERTIES:
    %   -> Anticipated: judgement values data object
    %   -> Unanticipated: judgement values data object
    %   -> modelHasAnticipatedShocks: true/false
    %   -> includesAnticipatedJudgements: true/false
    %   -> includesUnanticipatedJudgements: true/false
    %   -> isNonEmpty: true/false
    %
    % EVENTS:
    %   -> jaShocksObjStateChanged: broadcast on changes in the state of
    %      the anticipated shock judgements
    %   -> juShocksObjStateChanged: broadcast on changes in the state of
    %      the unanticipated shock judgements
    %
    % PUBLIC METHODS:
    %   -> applyJudgementToForecastRun: updates content of forecast run 
    %      with the judgements
    %
    % DETAILS:
    %   -> This m-file defines a shock judgements instructions object, 
    %      which can be used to update a MAPS forecast run dataset for new
    %      judgemental shock values as part of LSS model impose judgement 
    %      functionality.
    %   -> This class can only be instantiated as part of a
    %      Judgements.Instructions object.
    %   -> This object exposes "set" methods for both the Anticipated and
    %      Unanticipated properties that permits, for example, the
    %      following as a means for setting anticipated shock judgements on
    %      an instantiated object:
    %           jShocksObj.Anticipated = {'a' [1 NaN 1]};
    %      where the cell array input is a judgement data cell - see the
    %      Judgements.Data.Values class and its abstract super-class for 
    %      more details and an example describing judgement values cell 
    %      arrays and associated data.
    %   -> In addition, this object exposes all the public methods that
    %      form part of the Judgements.Data.Values class. These include
    %      "add", "overwrite" and "remove" method providing flexibility 
    %      over how the judgement data is set. This dual way of setting
    %      the data also provides continuity with earlier MAPS releases
    %      in which judgements were managed as structures.
    %   -> For example, the setting of anticipated shock judgements
    %      described above could be achieved in an equivalent way using the
    %      following method and input:
    %           jShocksObj.Anticipated.overwrite({'a' [1 NaN 1]});
    %      And the "add" and "remove" methods can be accessed in a similar
    %      way.
    %   -> This class also contains a method and the logic for updating the
    %      shocks in the forecast run dataset as part of the imposition of
    %      judgement - see the MAPS LSS model forecast run macros.
    %   -> Notice that an exception is thrown if the model being used is
    %      not forward-looking and users try to access setting methods 
    %      for anticipated shock judgements.
    %   -> The broadcast of the state change events facilitates cross
    %      judgement object validation. For example, users are not
    %      permitted to impose judgement on a shock and use it in an
    %      inversion at the same time (under the same anticipation or
    %      otherwise assumption). The event notification can be used to
    %      trigger validation (in this case a flag as a prompt) to check
    %      that the overall judgements instructions object remains valid.
    %
    % NOTES:
    %   -> Note the use of private properties for the anticipated and
    %      unanticipated shocks objects. The technique is a useful one for
    %      exposing an interface to users, while controlling the execution
    %      on "set" and in being free to initialise properties without
    %      being routed through the set method.
    %
    % This version: 09/12/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (Dependent=true)
        Anticipated
        Unanticipated
        includesAnticipatedJudgements
        includesUnanticipatedJudgements
        isNonEmpty
    end
    properties (SetAccess=private)
        modelHasAnticipatedShocks
    end
    properties (Access=private)       
        pAnticipated
        pUnanticipated
    end
    %% EVENTS
    events
        jaShocksObjStateChanged
        juShocksObjStateChanged
    end
    %% PUBLIC METHODS
    methods
        %% UPDATE FORECAST RUN
        function RunData = applyJudgementToForecastRun(jShocksObj,RunData)
            % This method updates a forecast run dataset for judgements.
            % Specifically, it updates the shock values for judgements
            % made.
            %
            % INPUTS:
            %   -> jShocksObj: shock judgements instructions object
            %   -> RunData: LSS model forecast run structure
            %
            % OUTPUTS:
            %   -> RunData: updated forecast run structure
            %
            %% CHECK INPUT
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),...
                    ':BadApplyJudgementToForecastRunNargin'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~isstruct(RunData)
                errId = ['MAPS:',mfilename('class'),...
                    ':BadApplyJudgementToForecastRunInput1'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% UPDATE ANTICIPATED SHOCKS (IF APPLICABLE)
            if jShocksObj.includesAnticipatedJudgements
                RunData.Forecast.Shocks.anticipated = jShocksObj...
                    .Anticipated.applyJudgementToForecastDataset(...
                    RunData.Forecast.Shocks.anticipated);
            end
            %% UPDATE UNANTICIPATED SHOCKS (IF APPLICABLE)
            if jShocksObj.includesUnanticipatedJudgements
                RunData.Forecast.Shocks.unanticipated = jShocksObj...
                    .Unanticipated.applyJudgementToForecastDataset(...
                    RunData.Forecast.Shocks.unanticipated);
            end
        end
        %% SET METHOD FOR ANTICIPATED SHOCKS
        function set.Anticipated(jShocksObj,jCell)
            if ~jShocksObj.modelHasAnticipatedShocks
                errId = ['MAPS:',mfilename('class'),':BadAnticipatedSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            try
                jShocksObj.pAnticipated.overwrite(jCell);
            catch InputValidationE
                errId = [...
                    'MAPS:',mfilename('class'),':BadAnticipatedInput'];
                generate_MAPS_exception_add_cause_and_throw(...
                    InputValidationE,errId);
            end  
        end
        %% SET METHOD FOR UNANTICIPATED SHOCKS
        function set.Unanticipated(jShocksObj,jCell)
            try
                jShocksObj.pUnanticipated.overwrite(jCell);
            catch InputValidationE
                errId = [...
                    'MAPS:',mfilename('class'),':BadUnanticipatedInput'];
                generate_MAPS_exception_add_cause_and_throw(...
                    InputValidationE,errId);
            end
        end
        %% GET METHOD FOR ANTICIPATED
        function ajObj = get.Anticipated(jShocksObj)
            ajObj = jShocksObj.pAnticipated;
        end
        %% GET METHOD FOR UNANTICIPATED
        function ujObj = get.Unanticipated(jShocksObj)
            ujObj = jShocksObj.pUnanticipated;
        end
        %% GET METHOD FOR ANTICIPATED SHOCK JUDGEMENT FLAG
        function ajIsNonEmpty = ...
                get.includesAnticipatedJudgements(jShocksObj)
            ajIsNonEmpty = (...
                jShocksObj.modelHasAnticipatedShocks&&...
                jShocksObj.Anticipated.isNonEmpty);
        end
        %% GET METHOD FOR UNANTICIPATED SHOCK JUDGEMENT FLAG
        function ujIsNonEmpty = ...
                get.includesUnanticipatedJudgements(jShocksObj)
            ujIsNonEmpty = jShocksObj.Unanticipated.isNonEmpty;
        end
        %% GET METHOD FOR ISNONEMPTY
        function jIsNonEmpty = get.isNonEmpty(jShocksObj)
            jIsNonEmpty = (...
                jShocksObj.includesAnticipatedJudgements||...
                jShocksObj.includesUnanticipatedJudgements);
        end
    end
    %% METHODS ACCESSIBLE BY JUDGEMENTS.INSTRUCTIONS CLASS
    methods (Access={?Judgements.Instructions})
        %% CONSTRUCTOR
        function jShocksObj = Shocks(Model,H)
            % Constructor method for this class.
            %
            % INPUTS:
            %   -> Model: LSS model structure
            %   -> H: forecast horizon scalar
            %
            % OUTPUTS:
            %   -> jShocksObj: instance of the object
            %
            %% UNPACK REQUIRED MODEL COMPONENTS
            [modelIsForwardLooking,zMnems] = unpack_model(...
                Model,{'modelIsForwardLooking','zMnems'});
            %% SET ANTICIPATED SHOCK FLAG PROPERTY
            jShocksObj.modelHasAnticipatedShocks = ...
                modelIsForwardLooking;
            %% INITIALISE UNANTICIPATED SHOCKS PROPERTY
            % The Unanticipated property is initialised as a values data
            % object. A listener is set for changes in the state of that
            % object which are broadcast by the data object.
            jShocksObj.pUnanticipated = Judgements.Data.Values(zMnems,H);
            addlistener(...
                jShocksObj.pUnanticipated,'jDataObjStateChanged',...
                @(Src,Evnt) jShocksObj.notifyForValidation(Src,Evnt,'u'));
            %% INITIALISE ANTICIPATED SHOCKS PROPERTY
            % This is done in the same way as for unanticipated shocks
            % except that the "modelisForwardLooking" flag dictates whether
            % or not editing the data is allowed. Notice that the same 
            % callback function is input to the addlistener method with the
            % two types of shock distinguished with a "u" and "a" flag.
            jShocksObj.pAnticipated = Judgements.Data.Values(...
                zMnems,H,~modelIsForwardLooking);
            addlistener(jShocksObj.pAnticipated,...
                'jDataObjStateChanged',@(Src,Evnt) ...
                jShocksObj.notifyForValidation(Src,Evnt,'a'));
        end
    end
    %% PRIVATE ACCESS METHODS
    methods (Access=private)
        %% FUNCTION TO BRAODCAST EVENT IN CALLBACK TO DATA OBJ EVENTS
        function notifyForValidation(jShocksObj,~,~,typeStr)
            % This method broadcasts an event as a listener callback.
            % The event is broadcast if either of the judgement values data
            % object's states change. See the constructor for details of
            % the listener sets.
            %
            % INPUTS:
            %   -> jShocksObj: shock judgements instructions object
            %   -> typeStr: either 'a' for anticipated or 'u' for
            %      unanticipated
            %
            % OUTPUTS:
            %   -> none
            %   
            %% NOTIFY FOR THE EVENT
            notify(jShocksObj,['j',typeStr,'ShocksObjStateChanged']);
        end
    end
end