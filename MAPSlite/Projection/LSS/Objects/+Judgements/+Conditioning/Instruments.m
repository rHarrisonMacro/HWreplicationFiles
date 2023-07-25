classdef Instruments < handle
    % This m-file defines the instruments part of a conditioning object.
    % It encapsulates all the information and logic around conditioning/
    % inversion instruments, but cannot be instantiated outside of the 
    % judgements conditioning instructions class.
    %
    % PUBLIC PROPERTIES:
    %   -> Anticipated: anticipated shock instruments data
    %   -> Unanticipated: unanticipated shock instruments data
    %   -> modelHasAnticipatedShocks: true/false    
    %   -> includesAnticipatedInstruments: true/false
    %   -> includesUnanticipatedInstruments: true/false
    %   -> isNonEmpty: true/false
    %   -> horizon: numeric scalar horizon up to which instruments have
    %      been specified
    %
    % EVENTS:
    %   -> jInstrumentsObjStateChanged: broadcast on the event that either
    %      of the instrument sets has changed
    %   -> jaInstrumentsObjStateChanged: broadcast on changes in the state
    %      of the anticipated shock instruments
    %   -> juInstrumentsObjStateChanged: broadcast on changes in the state
    %      of the unanticipated shock judgements
    %
    % PUBLIC METHODS:
    %   -> none
    %
    % DETAILS:
    %   -> This m-file defines a shock instruments object as part of a
    %      judgements conditioning object. It encapsulates all of the
    %      information about shock instruments, but cannot be instantiated
    %      outside of the Judgements.Conditioning.Instructions class 
    %      reflecting that it cannot be used without conditioning targets.
    %   -> See the Judgements.Conditioning.Instructions class for details
    %      of how it instantiates this class.
    %   -> Once this object has been instantiated it exposes the
    %      Anticipated and Unanticipated properties to users through "set"
    %      methods.
    %   -> These are in turn objects of the Judgements.Data.Instruments 
    %      class and this class exposes any public methods that exist in 
    %      that class. These include "add", "overwrite" and "remove" 
    %      methods providing flexibility over how the judgement data is 
    %      set. This dual way of setting the data also provides continuity
    %      with earlier MAPS releases in which judgements were managed as 
    %      structures.
    %   -> Notice that an exception is thrown if the model being used is
    %      not forward-looking and users try to access setting methods for
    %      anticipated shock instruments.
    %   -> The broadcast of the state change events facilitates cross
    %      judgement object validation. For example, users are not
    %      permitted to impose judgement on a shock and use it in an
    %      inversion at the same time (under the same anticipation or
    %      otherwise assumption). This is validated within the overall
    %      Judgements.Instructions object.
    %
    % NOTES:
    %   -> Note the use of private properties for the anticipated and
    %      unanticipated instruments objects. The technique is a useful one
    %      for exposing an interface to users, while controlling the
    %      execution on "set" and in being free to initialise properties
    %      without routing through the set method.
    %
    % This version: 25/11/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (Dependent=true)
        Anticipated
        Unanticipated
        includesAnticipatedInstruments
        includesUnanticipatedInstruments
        isNonEmpty
        horizon
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
       jInstrumentsObjStateChanged 
       jaInstrumentsObjStateChanged
       juInstrumentsObjStateChanged
    end
    %% PUBLIC METHODS
    methods
        %% SET METHOD FOR UNANTICIPATED INSTRUMENTS
        function set.Unanticipated(jInstrumentsObj,jCell)
            try
                jInstrumentsObj.pUnanticipated.overwrite(jCell);
            catch InputValidationE
                errId = [...
                    'MAPS:',mfilename('class'),':BadUnanticipatedInput'];
                generate_MAPS_exception_add_cause_and_throw(...
                    InputValidationE,errId);
            end
        end
        %% SET METHOD FOR ANTICIPATED INSTRUMENTS
        function set.Anticipated(jInstrumentsObj,jCell)
            if ~jInstrumentsObj.modelHasAnticipatedShocks
                errId = ['MAPS:',mfilename,':BadAnticipatedSet'];
                generate_and_throw_MAPS_exception(errId);
            end
            try
                jInstrumentsObj.pAnticipated.overwrite(jCell);
            catch InputValidationE
                errId = [...
                    'MAPS:',mfilename('class'),':BadAnticipatedInput'];
                generate_MAPS_exception_add_cause_and_throw(...
                    InputValidationE,errId);
            end
        end
        %% GET METHOD FOR UNANTICIPATED INSTRUMENTS
        function ujObj = get.Unanticipated(jInstrumentsObj)
            ujObj = jInstrumentsObj.pUnanticipated;
        end
        %% GET METHOD FOR ANTICIPATED INSTRUMENTS
        function ajObj = get.Anticipated(jInstrumentsObj)
            ajObj = jInstrumentsObj.pAnticipated;
        end
        %% GET METHOD FOR ANTICIPATED SHOCK JUDGEMENT FLAG
        function aiIsNonEmpty = ...
                get.includesAnticipatedInstruments(jInstrumentsObj)
            aiIsNonEmpty = (...
                jInstrumentsObj.modelHasAnticipatedShocks&&...
                jInstrumentsObj.Anticipated.isNonEmpty);
        end
        %% GET METHOD FOR UNANTICIPATED SHOCK JUDGEMENT FLAG
        function uiIsNonEmpty = ...
                get.includesUnanticipatedInstruments(jInstrumentsObj)
            uiIsNonEmpty = jInstrumentsObj.Unanticipated.isNonEmpty;
        end        
        %% GET METHOD FOR ISNONEMPTY
        function iIsNonEmpty = get.isNonEmpty(jInstrumentsObj)
            iIsNonEmpty = (...
                jInstrumentsObj.includesUnanticipatedInstruments||...
                jInstrumentsObj.includesAnticipatedInstruments);
        end
        %% GET METHOD FOR HORIZON
        function iHorizon = get.horizon(jInstrumentsObj)
            iHorizon = jInstrumentsObj.Unanticipated.horizon;
            if jTargetsObj.includesAnticipatedInstruments
                iHorizon = max(...
                    iHorizon,jInstrumentsObj.Anticipated.horizon);
            end
        end
    end
    %% PRIVATE METHODS TO JUDGEMENTS.CONDITIONING.INSTRUCTIONS OBJECTS
    methods (Access={?Judgements.Conditioning.Instructions})
    %% CONSTRUCTOR
        function jInstrumentsObj = Instruments(Model,H)
            % Constructor method for this class.
            %
            % INPUTS:
            %   -> Model: LSS model structure
            %   -> H: forecast horizon scalar
            %
            % OUTPUTS:
            %   -> jInstrumentsObj: instance of the object
            %
            %% UNPACK REQUIRED MODEL COMPONENTS
            [modelIsForwardLooking,zMnems] = unpack_model(...
                Model,{'modelIsForwardLooking','zMnems'});
            %% SET ANTICIPATED SHOCKS FLAG
            jInstrumentsObj.modelHasAnticipatedShocks = ...
                modelIsForwardLooking;
            %% INITIALISE UNANTICIPATED SHOCKS INSTRUMENTS
            % Note that a listener is set for changes in the state of the
            % judgement instruments data object. This is part of the
            % validation strategy for the Judgements.Instructions object -
            % see that object for details.
            jInstrumentsObj.pUnanticipated = ...
                Judgements.Data.Instruments(zMnems,H);
            addlistener(jInstrumentsObj.pUnanticipated,...
                'jDataObjStateChanged',@(Src,Evnt) ...
                jInstrumentsObj.notifyForValidation(Src,Evnt,'u'));
            %% INITIALISE ANTICIPATED SHOCKS INSTRUMENTS
            % This is done in the same way as for unanticipated shocks
            % except that the "modelisForwardLooking" flag dictates whether
            % or not editing the data is allowed. A listener for changes in
            % the state of the object is set analogous to that of 
            % unanticipated shocks above (note the use of the u/a flags).
            jInstrumentsObj.pAnticipated = Judgements.Data.Instruments(...
                zMnems,H,~modelIsForwardLooking);
            addlistener(jInstrumentsObj.pAnticipated,...
                'jDataObjStateChanged',@(Src,Evnt) ...
                jInstrumentsObj.notifyForValidation(Src,Evnt,'a'));
        end        
    end
    %% PRIVATE METHODS
    methods (Access=private)
        %% FUNCTION TO UPDATE VALIDATION FLAG IN CALLBACK TO UPDATE EVENTS
        function notifyForValidation(jInstrumentsObj,~,~,typeStr)
            % Callback function to broadcast in event of state changes.
            %
            % INPUTS:
            %   -> typeStr: either "a" for anticipated or "u" for
            %      unanticipated
            %
            % OUTPUTS:
            %   -> none
            %
            %% NOTIFY FOR CHANGE IN STATE OF ENTIRE INSTRUMENTS OBJECT
            notify(jInstrumentsObj,'jInstrumentsObjStateChanged');
            %% NOTIFY FOR CHANGES IN STATE OF SPECIFIC TYPE OF INSTRUMENT
            notify(jInstrumentsObj,...
                ['j',typeStr,'InstrumentsObjStateChanged']);
        end
    end
end