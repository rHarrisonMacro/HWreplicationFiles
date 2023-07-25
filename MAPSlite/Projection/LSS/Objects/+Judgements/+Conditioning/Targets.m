classdef Targets < handle
    % This m-file defines the targets part of a conditioning object.
    % It encapsulates all the information and business logic around 
    % conditioning/inversion targets, but cannot be instantiated 
    % outside of the judgements conditioning class.
    %
    % PUBLIC PROPERTIES:
    %   -> ModelVariables: judgements applied to model variables
    %   -> ModelObservables: judgements applied to model observables
    %   -> RawObservables: judgements applied to raw observables
    %   -> modelHasModelObservables: true/false
    %   -> modelHasRawObservables: true/false
    %   -> targetsAreDefinedAsMarginals: true/false
    %   -> AllVariables: all target judgements merged together
    %   -> includesModelVariableJudgements: true/false
    %   -> includesModelObservableJudgements: true/false
    %   -> includesRawObservableJudgements: true/false
    %   -> isNonEmpty: true/false
    %   -> horizon: numeric scalar horizon up to which judgements have
    %      been specified (across all variable types)
    %   -> isValidated: true/false
    %
    % EVENTS:
    %   -> jTargetsObjStateChanged: broadcast on the event that any of the
    %      target sets have changed
    %
    % PUBLIC METHODS:
    %   -> add: allows for judgements to be added one-by-one without having
    %      to specify the variable space (requires 2 inputs) or in bulk 
    %      (requires 1 input: a judgement cell array)
    %   -> overwrite: allows for all judgements to be overwritten without
    %      having to do so separately for each variable type
    %   -> remove: allows for any judgements to be removed without having
    %      to specify the variable type
    %
    % DETAILS:
    %   -> This m-file defines a conditioning targets object, which forms 
    %      part of a judgements conditioning instructions object. It 
    %      encapsulates all of the information about targets, but cannot be
    %      instantiated outside of the Judgements.Conditioning.Instructions
    %      class reflecting that it cannot be used without conditioning 
    %      instruments to go with the targets.
    %   -> See the Judgements.Conditioning.Instructions class for details 
    %      of how this class is instantiated.
    %   -> Once this object has been instantiated it exposes properties for
    %      each type of variable that can be targeted through "set" 
    %      methods: ModelVariables; ModelObservables; RawObservables.
    %   -> These target objects are in turn objects of either the 
    %      Judgements.Data.Values class or the Judgements.Data.Marginals
    %      class depending on construction. These classes expos any public
    %      methods that exist on them including "add", "overwrite" and
    %      "remove" methods to facilitate the creation of judgement data
    %      data information. For example, setting targets for model
    %      variables as part of conditioning judgements can be achieved
    %      identically in either of the following ways:
    %           jConditioiningObj.ModelVariables = {'piz' [<vecOfData>]};
    %           jConditioiningObj.ModelVariables.overwrite(...
    %               {'piz' [<vecOfData>]});
    %      And the "add"/"remove" methods that exist in the 
    %      Judgements.Data.Values class provide additional tools for the 
    %      setting of judgement data. 
    %   -> In addition, this object exposes public methods for the adding,
    %      overwriting and removal of target judgement data without having
    %      to explicitly access their type. For example, the setting of the
    %      model variable judgement above could also be achieved by:
    %           jConditioiningObj.overwrite({'piz' [<vecOfData>]});
    %   -> The interpretation of the data input depends on whether it is
    %      levels data or marginals data. If it is marginals data, the
    %      interpretation depends on the type of variable. For model
    %      variables and model observables the marginals are always treated
    %      as levels differences, whereas for raw observables their
    %      treatment depends on the nature of the data transformation
    %      equations as assertained by an LSS model content utility
    %      function.
    %   -> Notice that an exception is thrown if the model being used is 
    %      not compatible with raw/model observables and users try to 
    %      access setting methods for those.
    %   -> The broadcast of the state change event facilitates cross
    %      judgement object validation. In particular, there are rules
    %      around the relationship between targets and instruments that
    %      must be validated.
    %   -> This object also contains the logic for the merging of all
    %      targets into model variable space (which requires the
    %      translation of model observable and raw observable targets).
    %   -> Associated with that, is the logic for validating the object.
    %      Targets are valid if: (a) they can be merged into model variable
    %      space with no conflicting (overlapping) data; b) the raw
    %      observables can be numerically translated into model observable
    %      space. The latter requirement is, in effect, a requirement that
    %      the raw observable targets be specified from the start of the
    %      forecast horizon without any NaN gaps between non-NaN values.
    %      This reflects that data transformation functions typically
    %      convert levels to growth rates.
    %
    % NOTES:
    %   -> Note the use of private properties for the target objects. The 
    %      technique is a useful one for exposing an interface to users, 
    %      while controlling the execution on "set" and in being free to 
    %      initialise properties without routing through the set method.
    %   -> There is an awkwardness in validation of this object and the
    %      overall Judgements.Conditioning.Instructions object. This stems
    %      from the fact that the validity of raw observable judgements 
    %      depends on the data transformation equations and any time-
    %      varying trends data (as applicable to the model being used). For
    %      example, raw observables cannot be negative if the data
    %      transformation function includes the log operator. This means
    %      that changes in the state of a forecast run dataset being used
    %      alongside this object could invalidate its state. Unfortunately,
    %      run datasets are not objects so there is no way of guranteeing
    %      that they are valid or that they are consistent with the inputs
    %      used to instantiate this or any of the other judgement objects.
    %      The validation of this object works around part of the problem 
    %      by re-validating the object if the relevant parts of the run 
    %      dataset have changed since the object was previously validated.
    %      This hole could be eliminated if run datasets were converted to 
    %      objects. (Though it would still leave a validation hole because
    %      there would be nothing to stop the model being changed/
    %      invalidated during the object's life-cycle).
    %
    % This version: 09/12/2013
    % Author(s): Matt Waldron

    %% PROPERTIES
    properties (Dependent=true)
        ModelVariables
        ModelObservables
        RawObservables
        AllVariables
        includesModelVariableJudgements
        includesModelObservableJudgements
        includesRawObservableJudgements
        isNonEmpty   
        horizon
    end
    properties (SetAccess=private)
        modelHasModelObservables
        modelHasRawObservables
        targetsAreDefinedAsMarginals
        isValidated
    end
    properties (Access=private)
        modelHasTimeTrends
        xMnems
        Ymnems
        YtildeMnems
        allVarMnems
        H
        pModelVariables
        pModelObservables
        pRawObservables
        pAllVariables
        pRunData
    end   
    %% EVENTS
    events
        jTargetsObjStateChanged
    end       
    %% PUBLIC METHODS
    methods
        %% ADD METHOD
        function add(jTargetsObj,input1,input2)
            % The add methods allows data to be added regardless of type.
            %
            % INPUTS:
            %   -> input1: either a judgement cell array if 1 input is
            %      passed in or a mnemonic for the judgement if 2
            %   -> input2: a judgement row vector of values/logicals if 2
            %      inputs are passed in (non-existent otherwise!)
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK INPUTS & HANDLE INPUT ALTERNATIVES
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadAddNargin'];
                generate_and_throw_MAPS_exception(errId);
            elseif nargin < 3
                jCell = input1;
                errCode = 'BadAddSingleInput';
            else
                jCell = {input1 input2};
                errCode = 'BadAddDoubleInput';
            end
            %% CALL HELPER FUNCTION TO ADD THE DATA TO RELEVANT VAR TYPE
            % The helper looks-up the mnemonic and the associated variable
            % type, throwing an error if the mnemonic cannot be found. It
            % then adds the additional data relevant to each type found for
            % each data type in turn. The error code is supplied to produce
            % a relevant exception in the event that adding any of the data
            % leads to an error (eg if the data is not of the correct
            % type).
            jTargetsObj.updateTargetData(jCell,'add',errCode);
        end
        %% OVERWRITE METHOD
        function overwrite(jTargetsObj,jCell)
            % The overwrite method allows all data to be overwritten.
            %
            % INPUTS:
            %   -> jCell: a valid judgement data cell array
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK NUMBER OF INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadOverwriteNargin'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CALL HELPER FUNCTION TO OVERWRITE DATA FOR RELEVANT TYPES
            % See the note above in the "add" method for a description of
            % what the method called does.
            errCode = 'BadOverwriteInput1';
            jTargetsObj.updateTargetData(jCell,'overwrite',errCode);
        end        
        %% REMOVE METHOD
        function remove(jTargetsObj,jMnems)
            % The remove method allows data removal regardless of type.
            %
            % INPUTS:
            %   -> jMnems: column cell string array of mnemonics or a
            %      single mnemonic string
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK NUMBER OF INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadRemoveNargin'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CALL HELPER FUNCTION TO REMOVE DATA FOR RELEVANT TYPES
            % See the note above in the "add" method for a description of
            % what the method called does.
            errCode = 'BadRemoveInput1';
            jTargetsObj.updateTargetData(jMnems,'remove',errCode);             
        end
        %% SET METHOD FOR MODEL VARIABLES
        function set.ModelVariables(jTargetsObj,jCell)
            try
                jTargetsObj.pModelVariables.overwrite(jCell);
            catch InputValidationE
                errId = ['MAPS:',mfilename('class'),':BadModelVarInput'];
                generate_MAPS_exception_add_cause_and_throw(...
                    InputValidationE,errId);
            end
        end        
        %% SET METHOD FOR MODEL OBSERVABLES
        function set.ModelObservables(jTargetsObj,jCell)
            if ~jTargetsObj.modelHasModelObservables
                errId = ['MAPS:',mfilename('class'),':BadModelObsSet'];
                generate_and_throw_MAPS_exception(errId);                
            end
            try
                jTargetsObj.pModelObservables.overwrite(jCell);
            catch InputValidationE
                errId = ['MAPS:',mfilename('class'),':BadModelObsInput'];
                generate_MAPS_exception_add_cause_and_throw(...
                    InputValidationE,errId);
            end
        end        
        %% SET METHOD FOR RAW OBSERVABLES
        function set.RawObservables(jTargetsObj,jCell)
            if ~jTargetsObj.modelHasRawObservables
                errId = ['MAPS:',mfilename('class'),':BadRawObsSet'];
                generate_and_throw_MAPS_exception(errId);  
            end
            try
                jTargetsObj.pRawObservables.overwrite(jCell);
            catch InputValidationE
                errId = ['MAPS:',mfilename('class'),':BadRawObsInput'];
                generate_MAPS_exception_add_cause_and_throw(...
                    InputValidationE,errId);
            end
        end        
        %% GET METHOD FOR MODEL VARIABLES
        function xjObj = get.ModelVariables(jTargetsObj)
            xjObj = jTargetsObj.pModelVariables;
        end        
        %% GET METHOD FOR MODEL OBSERVABLES
        function YjObj = get.ModelObservables(jTargetsObj)
            YjObj = jTargetsObj.pModelObservables;
        end       
        %% GET METHOD FOR RAW OBSERVABLES
        function YtildejObj = get.RawObservables(jTargetsObj)
            YtildejObj = jTargetsObj.pRawObservables;
        end       
        %% GET METHOD FOR MODEL VARIABLES JUDGEMENT FLAG
        function xjIsNonEmpty = ...
                get.includesModelVariableJudgements(jTargetsObj)
           xjIsNonEmpty = jTargetsObj.ModelVariables.isNonEmpty;
        end       
        %% GET METHOD FOR MODEL OBSERVABLES JUDGEMENT FLAG
        function YjIsNonEmpty = ...
                get.includesModelObservableJudgements(jTargetsObj)
           YjIsNonEmpty = (...
               jTargetsObj.modelHasModelObservables&&...
               jTargetsObj.ModelObservables.isNonEmpty);
        end
        %% GET METHOD FOR RAW OBSERVABLES JUDGEMENT FLAG
        function YtildejIsNonEmpty = ...
                get.includesRawObservableJudgements(jTargetsObj)
           YtildejIsNonEmpty = (...
               jTargetsObj.modelHasRawObservables&&...
               jTargetsObj.RawObservables.isNonEmpty);
        end        
        %% GET METHOD FOR ISNONEMPTY
        function tIsNonEmpty = get.isNonEmpty(jTargetsObj)
            tIsNonEmpty = (...
                jTargetsObj.includesModelVariableJudgements||...
                jTargetsObj.includesModelObservableJudgements||...
                jTargetsObj.includesRawObservableJudgements);
        end   
        %% GET METHOD FOR HORIZON
        function tHorizon = get.horizon(jTargetsObj)
            tHorizon = jTargetsObj.ModelVariables.horizon;
            if jTargetsObj.includesModelObservableJudgements
                tHorizon = max(...
                    tHorizon,jTargetsObj.ModelObservables.horizon);
            end
            if jTargetsObj.includesRawObservableJudgements
                tHorizon = max(...
                    tHorizon,jTargetsObj.RawObservables.horizon);
            end
        end
        %% GET METHOD FOR MERGER OF ALL TARGET INFORMATION
        function AllTargets = get.AllVariables(jTargetsObj)
            if ~jTargetsObj.isValidated
                AllTargets = [];
            else
                AllTargets = jTargetsObj.pAllVariables;
            end
        end
    end
    %% PRIVATE METHODS TO JUDGEMENTS.CONDITIONING.INSTRUCTIONS OBJECTS
    methods (Access={?Judgements.Conditioning.Instructions})
        %% CONSTRUCTOR METHOD
        function jTargetsObj = Targets(Model,RunData,defineAsMarginals)   
            % Constructor method for this class. 
            % Note that it proceeds on the basis that it is being passed
            % valid inputs, which is guaranteed by the instructions class.
            %
            % INPUTS:
            %   -> Model: LSS model structure
            %   -> H: forecast horizon scalar
            %   -> defineAsMarginals: true/false
            %
            % OUTPUTS:
            %   -> jTargetsObj: instance of the object
            %
            %% SET MODEL CHARACTERISTICS FLAGS
            [modelHasMeasurementEqs,modelHasDataTransformationEqs,...
                modelHasTimeVaryingTrends] = ...
                unpack_model(Model,{'modelHasMeasurementEqs',...
                'modelHasDataTransformationEqs',...
                'modelHasTimeVaryingTrends'});
            jTargetsObj.modelHasModelObservables = modelHasMeasurementEqs;
            jTargetsObj.modelHasRawObservables = ...
                modelHasDataTransformationEqs;   
            jTargetsObj.modelHasTimeTrends = modelHasTimeVaryingTrends;
            %% EXTRACT FORECAST HORIZON FROM RUN DATA
            horizon = size(RunData.Forecast.Shocks.unanticipated,2);
            %% INITIALISE MODEL VARIABLE CONDITIONING TARGETS
            % Note that a listener is added for changes in the state of the
            % judgement values data object. This is used to facilitate
            % validation of this object and the overall
            % Judgements.Conditioning.Instructions object. Note also that
            % the initialisation depends on whether the judgement data is
            % to be defined in marginals or levels space (the default). If
            % in marginals, by the definition of model variables, it is
            % assumed that they are defined in levels differences.
            modVarMnems = unpack_model(Model,{'xMnems'});
            allMnems = modVarMnems;
            jTargetsObj.xMnems = modVarMnems;
            if defineAsMarginals
                xf = RunData.Forecast.modelVariables;
                jTargetsObj.pModelVariables = ...
                    Judgements.Data.Marginals(...
                    modVarMnems,xf,true(size(xf,1),1));       
            else
                jTargetsObj.pModelVariables = ...
                    Judgements.Data.Values(modVarMnems,horizon);
            end
            addlistener(jTargetsObj.pModelVariables,...
                'jDataObjStateChanged',...
                @jTargetsObj.updateFlagAndNotifyForValidation);
            %% INITIALISE MODEL OBS CONDITIONING TARGETS
            % A listener is set for validation in the same was as above.
            % Note that if the model does not have model observables, then
            % the model observables object is instantiated using an empty
            % set of mnemonics and cannot be edited.
            if modelHasMeasurementEqs
                modelObsMnems = unpack_model(Model,'Ymnems');
                allMnems = [allMnems;modelObsMnems];
                jTargetsObj.Ymnems = modelObsMnems;
            else
                modelObsMnems = {''};
            end
            if defineAsMarginals && modelHasMeasurementEqs
                Yf = RunData.Forecast.modelObservables; 
                jTargetsObj.pModelObservables = ...
                    Judgements.Data.Marginals(...
                    modelObsMnems,Yf,true(size(Yf,1),1),...
                    ~modelHasMeasurementEqs);
            else
                jTargetsObj.pModelObservables = Judgements.Data.Values(...
                    modelObsMnems,horizon,~modelHasMeasurementEqs);
            end
            addlistener(jTargetsObj.pModelObservables,...
                'jDataObjStateChanged',...
                @jTargetsObj.updateFlagAndNotifyForValidation);
            %% INITIALISE RAW OBS CONDITIONING TARGETS (IF APPLICABLE)
            % A listener is set for validation in the same was as above.
            % Note that if the model does not have raw observables, then
            % the raw observables object is instantiated using an empty set
            % of mnemonics and cannot be edited. Notice that the space for
            % the marginals (if applicable) is determined by the content of
            % the model and the nature of the data transformation.
            if modelHasDataTransformationEqs
                rawObsMnems = unpack_model(Model,'YtildeMnems');
                allMnems = [allMnems;rawObsMnems];
                jTargetsObj.YtildeMnems = rawObsMnems;
            else
                rawObsMnems = {''};
            end
            if defineAsMarginals && modelHasDataTransformationEqs
                Ytildef = RunData.Forecast.rawObservables;
                isStationary = ...
                    determine_stationarity_of_raw_observables(Model);                
                jTargetsObj.pRawObservables = Judgements.Data.Marginals(...
                    rawObsMnems,Ytildef,isStationary,...
                    ~modelHasDataTransformationEqs);
            else
                jTargetsObj.pRawObservables = Judgements.Data.Values(...
                    rawObsMnems,horizon,~modelHasDataTransformationEqs);
            end
            addlistener(jTargetsObj.pRawObservables,...
                'jDataObjStateChanged',...
                @jTargetsObj.updateFlagAndNotifyForValidation);
            %% INITIALISE ALL OTHER PROPERTIES
            jTargetsObj.allVarMnems = allMnems;
            jTargetsObj.H = horizon;
            jTargetsObj.targetsAreDefinedAsMarginals = defineAsMarginals;
            jTargetsObj.isValidated = true;
            jTargetsObj.pAllVariables = Judgements.Data.Values(...
                modVarMnems,horizon);
            jTargetsObj.pRunData = RunData;
        end
        %% VALIDATE METHOD
        function validate(jTargetsObj,Model,RunData)
            % Validation method for conditioning targets object
            %
            % INPUTS:
            %   -> jTargetsObj: conditioning targets object
            %   -> Model: LSS model structure
            %   -> RunData: LSS model forecast run data structure
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK FOR CHANGES IN FORECAST RUN DATA
            % This check is necessary because the target object must be
            % revalidated if the forecast run dataset structure changes.
            % A previously validated conditioning object could become
            % invalidated if the time trends are updated in such a way as
            % to make judgements made in raw observable space invalid. The
            % run dataset is not an object, so there is no way of listening
            % for changes to its state. Moreover, if the judgement data is
            % defined in maginal space, then changes to the run data object
            % necessitate an update of the base data defined in the
            % marginals data object (since ultimately all judgements must
            % be in levels).
            if jTargetsObj.isNonEmpty && ...
                    ~isequal(jTargetsObj.pRunData,RunData)
                if jTargetsObj.targetsAreDefinedAsMarginals
                    jTargetsObj...
                        .updateBaseDataForTargetsDefinedAsMarginals(...
                        RunData);
                end
                jTargetsObj.pRunData = RunData;
                    jTargetsObj.isValidated = false;
            end
            %% EXIT THIS FUNCTION IF OBJECT IS ALREADY VALIDATED
            % Note that if the target set is empty, then this flag is
            % guaranteed to be set to true.
            if jTargetsObj.isValidated
                return
            end
            %% SET VALIDATION FLAGS
            isROtargetSetValid = true;
            isMergedTargetSetValid = true;
            %% SETUP INDICES & MATRIX FOR COMBINED SET OF JUDGEMENTS
            % These are added to below for each of the variable types in
            % turn.
            jCombinedInds = zeros(0,1);
            jCombinedMat = NaN*ones(0,jTargetsObj.H);
            %% VALIDATE & TRANSFORM RO TARGET SET (IF NECESSARY)
            % Notice that if the RO target set is invalid, then an
            % exception is caught (to use in an overall validation
            % exception constructed below) and the validation of the
            % combined set continues. This ensures that users receive all
            % validation errors at the same time.
            if jTargetsObj.includesRawObservableJudgements
                try
                    [jCombinedInds,jCombinedMat] = jTargetsObj...
                        .transformRawObsTargetsToModelVarSpace(...
                        Model,RunData);
                catch ROtargetsE
                    isROtargetSetValid = false;
                    jCombinedInds = ...
                        transform_indices_from_raw_to_model_observable_space(...
                        Model,jTargetsObj.RawObservables.inds);
                    jCombinedMat = jTargetsObj.RawObservables.mat;
                end
            end
            %% TRANSFORM & ADD MO TARGET SET TO COMBINED SET (IF NECESSARY)
            if jTargetsObj.includesModelObservableJudgements              
                [YjIndsTr,YjMatTr] = jTargetsObj...
                    .transformModelObsTargetsToModelVarSpace(Model);
                jCombinedInds = [jCombinedInds;YjIndsTr];
                jCombinedMat = [jCombinedMat;YjMatTr];
            end
            %% ADD MV TARGET SET TO COMBINED SET (IF NECESSARY)
            if jTargetsObj.includesModelVariableJudgements
                jCombinedInds = [jCombinedInds;
                    jTargetsObj.ModelVariables.inds];
                jCombinedMat = [jCombinedMat;...
                    jTargetsObj.ModelVariables.mat];
            end
            %% CREATE COMBINED JUDGEMENT CONDITIONING TARGETS CELL ARRAY
            % The indices created above are used to set the combined
            % target mnemonics (which are in model variable space) and the
            % matrix is used to create a set of vectors. These are combined
            % to form a judgement cell array.
            jCombinedMnems = jTargetsObj.xMnems(jCombinedInds);
            njCombined = size(jCombinedMnems,1);
            jCombinedVecs = mat2cell(jCombinedMat,ones(njCombined,1));
            jCombinedCell = [jCombinedMnems jCombinedVecs];
            %% CREATE COMBINED TARGETS OBJECT
            % This operation will throw an exception if there are any
            % overlapping/conflicting targets.
            try
                jTargetsObj.pAllVariables = Judgements.Data.Values(...
                    jTargetsObj.xMnems,jTargetsObj.H);
                jTargetsObj.pAllVariables.overwrite(jCombinedCell);
            catch MergerE
                isMergedTargetSetValid = false;
            end
            %% THROW EXCEPTION IF EITHER VALIATION RULE IS BROKEN
            if ~isROtargetSetValid || ~isMergedTargetSetValid
                masterErrId = [...
                    'MAPS:',mfilename('class'),':InvalidTargets'];
                InvalidTargetsE = generate_MAPS_exception(masterErrId);
                if ~isROtargetSetValid
                    InvalidTargetsE = addCause(InvalidTargetsE,ROtargetsE);
                end
                if ~isMergedTargetSetValid
                    InvalidTargetsE = addCause(InvalidTargetsE,MergerE);
                end
                throw(InvalidTargetsE);
            end
            %% RESET VALIDATION FLAG TO TRUE IF TARGETS WERE VALIDATED
            jTargetsObj.isValidated = true;
        end
    end
    %% PRIVATE METHODS
    methods (Access=private)
        %% FUNCTION TO TRANSFORM RAW OBSERVABLE TARGETS TO MODEL VAR SPACE
        function [YtildejIndsTr2,YtildejMatTr2] = ...
                transformRawObsTargetsToModelVarSpace(...
                jTargetsObj,Model,RunData)    
            % Function to convert RO judgements to MV space.
            %
            % INPUTS:
            %   -> jTargetsObj: conditioning targets object
            %   -> Model: LSS model structure
            %   -> RunData: LSS model forecast run data structure
            %
            % OUTPUTS:
            %   -> YtildejIndsTr2: indices of RO judgements transformed to
            %      MV space
            %   -> YtildejMatTr2: matrix of RO judgement values transformed
            %      to MV space
            %
            %% TRANSFORM RO TARGETS TO MO SPACE
            [YtildejIndsTr,YtildejMatTr] = ...
                jTargetsObj.transformRawObsTargetsToModelObsSpace(...
                Model,RunData);
            %% TRANSFORM TRANSFORMED RO TARGETS TO MV SPACE
            [YtildejIndsTr2,YtildejMatTr2] = ...
                jTargetsObj.transformModelObsTargetsToModelVarSpace(...
                Model,YtildejIndsTr,YtildejMatTr);   
        end
        %% FUNCTION TO TRANSFORM RAW OBSERVABLE TARGETS TO MODEL OBS SPACE
        function [YtildejIndsTr,YtildejMatTr] = ...
                transformRawObsTargetsToModelObsSpace(...
                jTargetsObj,Model,RunData)       
            % Function to convert RO judgements to MO space.
            %
            % INPUTS:
            %   -> jTargetsObj: conditioning targets object
            %   -> Model: LSS model structure
            %   -> RunData: LSS model forecast run data structure
            %
            % OUTPUTS:
            %   -> YtildejIndsTr: indices of RO judgements transformed to
            %      MO space
            %   -> YtildejMatTr: matrix of RO judgement values transformed
            %      to MO space
            %
            %% FIND NAN ELEMENTS OF RAW OBSERVABLES JUDGEMENT DATA
            YtildejMatLogicals = jTargetsObj.RawObservables.matLogicals;
            YtildejIsNaN = ~YtildejMatLogicals;
            %% FIND INDICES OF INVALID RO JUDGEMENTS & THROW EXCEPTION
            % RO judgements are invalid if they do not start from beginning
            % of forecast horizon or are discontinuous at any point after
            % that. The reason for this is that data transformation
            % equations typically include the first difference of ROs and
            % transformations to MO space are undefined in cases where
            % numeric judgements appear after any NaN values.
            invalidROtargetInds = (...
                YtildejIsNaN(:,1)|any(diff(YtildejIsNaN,1,2)==-1,2));
            if any(invalidROtargetInds)
                errId = ['MAPS:',mfilename('class'),':BadROtargets'];
                generate_MAPS_exception_add_causes_and_throw(errId,...
                    jTargetsObj.RawObservables.mnems,invalidROtargetInds);
            end
            %% TRANSFORM INDICES FROM RAW TO MODEL OBSERVABLE SPACE
            YtildejInds = jTargetsObj.RawObservables.inds;
            YtildejIndsTr = ...
                transform_indices_from_raw_to_model_observable_space(...
                Model,YtildejInds); 
            %% UNPACK DATA TRANSFORMATION FUNCTION HANDLE
            DTfunHandle = unpack_model(Model,'DTfunHandle');
            %% EXTRACT RO & TT INITIAL CONDITIONS & TT FORECAST DATA
            YtildeT = RunData.Constraint.rawObservables;
            if jTargetsObj.modelHasTimeTrends
                etatT = RunData.Constraint.timeVaryingTrends;
                etatf = RunData.Forecast.timeVaryingTrends; 
            end
            %% CREATE A COMPLETE MATRIX OF RO DATA FOR TRANSFORMATION
            nY = size(YtildeT,1);
            YtildejMatFull = NaN*ones(nY,jTargetsObj.H);
            %% INPUT RO JUDGEMENTS INTO THAT MATRIX
            YtildejMat = jTargetsObj.RawObservables.mat;            
            YtildejMatFull(YtildejInds,:) = YtildejMat;
            %% TRANSFORM RO JUDGEMENTS TO MO SPACE
            if jTargetsObj.modelHasTimeTrends
                YtildejMatFullTr = ...
                    transform_observables_from_raw_to_model_space(...
                    DTfunHandle,YtildejMatFull,YtildeT,etatf,etatT);
            else
                YtildejMatFullTr = ...
                    transform_observables_from_raw_to_model_space(...
                    DTfunHandle,YtildejMatFull,YtildeT);
            end
            %% EXTRACT RELEVANT ROWS OF FULL MATRIX USING TRANSFORMED INDS
            YtildejMatTr = YtildejMatFullTr(YtildejIndsTr,:);
            %% VALIDATE TRANSFORMED RO JUDGEMENTS
            % This uses a MAPS LSS model projection helper, which requires
            % the transformed MO mnemonics and a vector of horizons for 
            % each of the separate transformed RO judgement vectors. (If 
            % LSS model forecast run execution were completely OO, then it
            % is likely that this could be factored out into this method 
            % (since a valid judgements instructions object guarantees that
            % the judgement content is valid)).
            nYj = size(YtildejMatLogicals,1);
            SYtildejs = NaN*ones(nYj,1);
            for iYj = 1:nYj
                SYtildejs(iYj) = find(YtildejMatLogicals(iYj,:),1,'last');
            end
            [~,SYjsSortInds] = sort(YtildejIndsTr);
            SYjs = SYtildejs(SYjsSortInds);
            YjMnems = jTargetsObj.Ymnems(YtildejIndsTr);
            validate_transformed_model_observable_judgements(...
                YtildejMatTr,YjMnems,SYjs);            
        end
        %% FUNCTION TO TRANSFORM MODEL OBS TARGETS TO MODEL VAR SPACE
        function [YjIndsTr,YjMatTr] = ...
                transformModelObsTargetsToModelVarSpace(...
                jTargetsObj,Model,YjInds,YjMat)
            % Function to convert MO judgements to MV space.
            %
            % INPUTS:
            %   -> jTargetsObj: conditioning targets object
            %   -> Model: LSS model structure
            %   -> YjInds: indices of the model observable judgements
            %   -> YjMat: model observable judgement values matrix
            %
            % OUTPUTS:
            %   -> YjIndsTr: indices of MO judgements transformed to MV
            %      space
            %   -> YtildejMatTr: matrix of MO judgement values transformed
            %      to MV space
            %
            %% HANDLE ALTERNATIVE INPUT USE CASE
            % This facilitates the function call in two different cases: a)
            % transformation of RO judgements that have already been
            % transformed into MO space, in which case 4 inputs are passed
            % in; b) transformation of MO judgements, in which 2 inputs are
            % passed in.
            if nargin < 3
                YjInds = jTargetsObj.ModelObservables.inds;
                YjMat = jTargetsObj.ModelObservables.mat;
            end  
            %% TRANSFORM INDICES FROM MO TO MV SPACE
            YjIndsTr = ...
                transform_indices_from_observable_to_model_var_space(...
                Model,YjInds);
            %% TRANSFORM JUDGEMENT VALUES FROM MO TO MV SPACE
            [D,G] = unpack_model(Model,{'D','G'});
            YjMatTr = (YjMat-D(YjInds)*ones(1,jTargetsObj.H))...
                ./(diag(G(YjInds,YjIndsTr))*ones(1,jTargetsObj.H));  
        end 
        %% FUNCTION TO LOOKUP VARIABLE TYPE & UPDATE JUDGEMENT DATA
        function updateTargetData(jTargetsObj,jCell,methodStr,errCode)
            % Helper function to update target data.
            % Used as part of the "add", "overwrite", "remove" public
            % methods in this class.
            %
            % INPUTS:
            %   -> jTargetsObj: judgement conditioning targets obj
            %   -> jCell: judgement data cell array, column cell string
            %      array of mnemomics or a single mnemomic string
            %   -> methodStr: method on the judgement data values class
            %      (either "add", "overwrite" or "remove")
            %   -> errCode: code used to create exception in event that
            %      exception is thrown by judgement data method.
            %
            % OUTPUTS:
            %   -> none
            %
            % NOTES:
            %   -> Although the object's state is reverted to its state on
            %      input if any of the data update methods throws an
            %      exception, this does not stop notification events being
            %      fired in cases where the object has been partially
            %      updated. This has implications for the status of
            %      validation in the Conditioning.Instructions object. It
            %      would be possible to defined a new "dataReset" event (or
            %      something similarly named) to be triggered in this
            %      function in such cases with a listener defined in that
            %      class to deal with the consequences. My view was that
            %      the additional complexity and cost of implementation
            %      outweighs the likely benefits (given that the main
            %      computational cost of validation is the validation of
            %      this object, which this class manages correctly). See
            %      below for more commentary on the exception cases.
            %
            %% ATTEMPT DATA UPDATE
            % Note that an exception is thrown on the first error that is 
            % encountered, meaning that further errors are not uncovered.
            % Note also that this function ensures that the object is 
            % returned to it's previous state in those circumstances.
            isInputObjValidated = jTargetsObj.isValidated;
            try 
                %% CONVERT JCELL TO CELL IF IT IS A STRING
                % This is necessary in the case that the "remove" method
                % was invoked with a single string input.
                if ischar(jCell)
                    jCell = {jCell};
                end
                %% SET FLAGS TO KEEP TRACK OF WHICH DATA HAS BEEN UPDATED
                % This is necessary so that the data can be reset in the
                % event that an exception is thrown.
                isMVjudgementDataUpdated = false;
                isMOjudgementDataUpdated = false;                
                %% GET VARIABLE TYPE LOGICALS
                [xLogicals,Ylogicals,YtildeLogicals] = jTargetsObj...
                    .getVariableTypeLogicalsForInputMnemonics(jCell(:,1));
                %% UPDATE MODEL VARIABLE DATA (IF APPLICABLE)   
                % Extract the judgement data cell array prior to updating
                % the data so that the object can be reset to its previous
                % state in the event an exception is thrown below.
                if any(xLogicals)
                    xjCell = jTargetsObj.ModelVariables.cellArray;
                    jTargetsObj.ModelVariables...
                        .(methodStr)(jCell(xLogicals,:));                    
                    isMVjudgementDataUpdated = true;
                end
                %% UPDATE MODEL OBSERVABLE DATA (IF APPLICABLE)  
                if any(Ylogicals)
                    YjCell = jTargetsObj.ModelObservables.cellArray;
                    jTargetsObj.ModelObservables...
                        .(methodStr)(jCell(Ylogicals,:));
                    isMOjudgementDataUpdated = true;
                end
                %% UPDATE RAW OBSERVABLE DATA (IF APPLICABLE)  
                if any(YtildeLogicals)
                    jTargetsObj.RawObservables...
                        .(methodStr)(jCell(YtildeLogicals,:));
                end
                %% HANDLE EXCEPTION
                % Notice that the model variable judgement data is reset to
                % the same state as on input to this function in cases 
                % where the update of the model observable or raw 
                % observable judgement data caused an exception and that 
                % the model observable judgement data is reset to the input
                % state in cases where the update of raw observable
                % judgement data caused an exception. In the event of any
                % exception, the validation flag is reset to input state.
            catch ValidationE
                if isMVjudgementDataUpdated
                    jTargetsObj.ModelVariables.overwrite(xjCell);
                end
                if isMOjudgementDataUpdated
                    jTargetsObj.ModelObservables.overwrite(YjCell);
                end
                jTargetsObj.isValidated = isInputObjValidated;
                errId = ['MAPS:',mfilename('class'),':',errCode];
                generate_MAPS_exception_add_cause_and_throw(...
                    ValidationE,errId);
            end
        end
        %% FUNCTION TO GET VARIABLE TYPE INDICES
        function [xLogicals,Ylogicals,YtildeLogicals] = ...
                getVariableTypeLogicalsForInputMnemonics(...
                jTargetsObj,inputMnems)
            % Helper function to get variable type mnemonics.
            % Used as part of the "add", "overwrite", "remove" public
            % methods in this class.
            %
            % INPUTS:
            %   -> jTargetsObj: judgement conditioning targets obj
            %   -> inputMnems: mnemonics input by user
            %
            % OUTPUTS:
            %   -> xLogicals: logicals indicating which of the input mnems
            %      is a model variable
            %   -> Ylogicals: logicals indicating which of the input mnems
            %      is a model observable
            %   -> xLogicals: logicals indicating which of the input mnems
            %      is a raw observable
            %
            %% VALIDATE THAT INPUT MNEMONICS BELONG TO MODEL
            errId = ['MAPS:',mfilename('class'),':InvalidMnems'];
            validate_strings_belong_to_reference_set(...
                inputMnems,jTargetsObj.allVarMnems,errId);
            %% SET MODEL VARIABLE LOGICALS
            xLogicals = ismember(inputMnems,jTargetsObj.xMnems);
            %% SET MODEL OBSERVABLE LOGICALS 
            Ylogicals = false(size(inputMnems,1),1);
            if jTargetsObj.modelHasModelObservables
                Ylogicals = ismember(inputMnems,jTargetsObj.Ymnems);
            end
            %% SET RAW OBSERVABLE MNEMONICS
            YtildeLogicals = false(size(inputMnems,1),1);
            if jTargetsObj.modelHasRawObservables
                YtildeLogicals = ismember(...
                    inputMnems,jTargetsObj.YtildeMnems);
            end
        end
        %% FUNCTION TO UPDATE TARGETS DEFINED AS MARGINALS FOR NEW RUN DATA
        function updateBaseDataForTargetsDefinedAsMarginals(...
                jTargetsObj,RunData)
            % Function to update base data in judgement marginals defs.
            % It should only be called if the targets are defined as
            % marginals.
            %
            % INPUTS:
            %   -> RunData: forecast run data structure
            %
            % OUTPUTS:
            %   -> none
            %
            %% UPDATE BASE DATA FOR MODEL VARIABLES
            jTargetsObj.pModelVariables.updateBaseDataset(...
                RunData.Forecast.modelVariables);
            %% UPDATE BASE DATA FOR MODEL OBSERVABLES
            if jTargetsObj.modelHasModelObservables
                jTargetsObj.pModelObservables.updateBaseDataset(...
                RunData.Forecast.modelObservables);
            end
            %% UPDATE BASE DATA FOR RAW OBSERVABLES
            if jTargetsObj.modelHasRawObservables
                jTargetsObj.pRawObservables.updateBaseDataset(...
                RunData.Forecast.rawObservables);
            end
        end
        %% FUNCTION TO UPDATE VALIDATION FLAG IN CALLBACK TO UPDATE EVENTS
        function updateFlagAndNotifyForValidation(jTargetsObj,~,~)
            % Callback function to handle changes in state of data objects.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% BROADCAST EVENT
            % Notify users of this object that its state has changed.
            notify(jTargetsObj,'jTargetsObjStateChanged');
            %% RESET VALIDATION FLAG
            jTargetsObj.isValidated = false;
            %% HANDLE SPECIAL CASE IN WHICH STATE CHANGES HAVE EMPTIED OBJ
            % If the object is now completely empty, then it is validated
            % and the combined set of targets is reinitialised to empty.
            if ~jTargetsObj.isNonEmpty
                jTargetsObj.isValidated = true;
                jTargetsObj.pAllVariables = ...
                    Judgements.Data.Values(...
                    jTargetsObj.xMnems,jTargetsObj.H);
            end
        end    
    end
end