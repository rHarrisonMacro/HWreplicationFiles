classdef Marginals < Judgements.Data.Values
    % This m-file defines a marginal values judgement data class.
    % This header only defines and discusses aspects of the class that are
    % not included and discussed in the Values judgement data class and its
    % abstract super-class.
    %
    % PUBLIC PROPERTIES:
    %   -> isLevelsDifference: nVars*1 logical vector (where false
    %      indicates that the marginal to apply is a percentage difference)
    %   -> baseMat: nVars*H matrix of base data on which to apply the
    %      marginals
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> constructor: requires a set of model mnemonics, base forecast
    %      dataset, information about whether marginals are LD or PD (on a
    %      variable-by-variable) basis and, optionally, isLockedForEditing
    %      flag & returns an empty judgement data object
    %   -> applyJudgementToForecastDataset: applies the judgement on the
    %      object to the base data input on construction (and can 
    %      optionally be called with a new base data input in which case it
    %      first calls the updateBaseDataset method).
    %   -> updateBaseDataset: allows for the base dataset (matrix) to be 
    %      updated   
    %
    % DETAILS:
    %   -> The marginals judgement data class is used as the basis for
    %      judgement data to be defined in marginal space. This is useful,
    %      for example, in the definition of fix response instructions.
    %   -> This m-file depends on the Value judgement data class and its 
    %      interface which is defined in an abstract super-class. See the 
    %      super-class header for more details. This class overwrites a
    %      subset of the methods in the Values class, allowing users to
    %      define judgement data in marginal space, rather than absolute
    %      space.
    %      In order to instantiate this sub-class, the constructor must be
    %      called with model mnemonics, a base dataset on which to impute
    %      the marginals and information (on a variable-by-variable basis)
    %      as to the type of marginal (either LD (true) or PD (false)).
    %      Optionally, the constructor may also be called with a third,
    %      logical scalar input dictating whether or not the object is
    %      locked for editing. This is useful because not all models
    %      support all types of judgement. These inputs are validated in 
    %      the super-class rather than below (on the DRY principle).
    %   -> This class overwrites the value class in two ways. First, it 
    %      overrides the method that computes the merged matrix of numeric
    %      judgement data so that the marginal judgement information is
    %      converted to levels on the base data. Second, it over-rides the
    %      applyJudgementToForecastDataset method, reflecting that the
    %      judgement data is dependent on the base data and so should be
    %      updated if the base data has changed. Associated with that, it
    %      adds a public method that allows for the update of the base
    %      data (to avoid forcing users to redefine marginal judgements in
    %      the event that the same marginal judgement apply to forecast run
    %      datasets with different base levels).
    %
    % NOTES:
    %   -> None
    %
    % This version: 09/12/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (SetAccess=private)
        isLevelsDifference
        baseMat
    end
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR
        function jMargValsDataObj = Marginals(...
                modelMnems,baseMat,isLevelsDifference,isLockedForEditing)
            % This is the constructor method.
            %
            % INPUTS:
            %   -> modelMnems: column cell array of mnemonic strings
            %   -> baseMat: two-dimensional numeric matrix
            %   -> isLevelsDifference: logical column vector
            %   -> isLockedForEditing (optional): true/false
            %
            % OUTPUTS:
            %   -> jMargValsDataObj: instance of the object
            %
            %% CHECK INPUTS
            if nargin < 3
                errId = ['MAPS:',mfilename('class'),':BadConstructNargin'];
                generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
            elseif ~is_finite_real_two_dimensional_numeric_matrix(baseMat)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput2'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% HANDLE OPTIONAL EDITING LOCK FLAG INPUT
            if nargin < 4
                isLockedForEditing = false;
            end
            %% CALL THE SUPER-CLASS CONSTRUCTOR
            % Note that input validation is done in the super-class
            % constructor.
            H = size(baseMat,2);
            jMargValsDataObj@Judgements.Data.Values(...
                modelMnems,H,isLockedForEditing);
            %% VALIDATE REMAINING INPUTS & CONSISTENCY WITH EACH OTHER
            if ~is_logical_column_vector(isLevelsDifference)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput3'];
                generate_and_throw_MAPS_exception(errId);
            elseif size(modelMnems,1) ~= size(baseMat,1)
                errId = ['MAPS:',mfilename('class'),...
                    ':InconsistentConstructInput1Inpu2'];
                generate_and_throw_MAPS_exception(errId);
            elseif size(modelMnems,1) ~= size(isLevelsDifference,1)
                errId = ['MAPS:',mfilename('class'),...
                    ':InconsistentConstructInput1Inpu3'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% ADD ADDITIONAL MARGINALS JUDGEMENT DATA PROPERTIES
            jMargValsDataObj.isLevelsDifference = isLevelsDifference;
            jMargValsDataObj.baseMat = baseMat;           
        end
        %% APPLY JUDGEMENT TO FORECAST DATASET
        function fValsMatUpdated = ...
                applyJudgementToForecastDataset(jMargValsDataObj,fValsMat)
            % This method update a forecast dataset with the judgements.
            %
            % INPUTS:
            %   -> jValsDataObj: judgement marginal values data object
            %   -> fValsMat (optional): matrix of forecast data to update
            %
            % OUTPUTS:
            %   -> fValsMatUpdated: forecast data updated for judgements
            %
            %% CHECK INPUT DATA TO SEE IF BASE DATA SHOULD BE UPDATED
            if nargin > 1
               if ~isequal(fValsMat,jMargValsDataObj.baseMat)
                   jMargValsDataObj.updateBaseDataset(fValsMat)
               end
            end
            %% INITIALISE OUTPUT
            fValsMatUpdated = jMargValsDataObj.baseMat;
            %% UPDATE FORECAST DATASET USING INDICES, LOGICALS & VALUES
            jInds = jMargValsDataObj.inds;
            jValsMatLogicals = jMargValsDataObj.matLogicals;
            jValsMat = jMargValsDataObj.mat;
            for t = 1:jMargValsDataObj.horizon
                fValsMatUpdated(jInds(jValsMatLogicals(:,t)),t) = ...
                    jValsMat(jValsMatLogicals(:,t),t);
            end
        end
        %% UPDATE BASE DATASET
        function updateBaseDataset(jMargValsDataObj,fValsMat)
            % This method update a forecast dataset with the judgements.
            %
            % INPUTS:
            %   -> jMargValsDataObj: judgement marginal values data object
            %   -> fValsMat: matrix of forecast data to update base with
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK INPUT
            % An alternative implementation here would be to restrict
            % access to particular classes.
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),...
                    ':BadUpdateBaseDataNargin'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~is_finite_real_two_dimensional_numeric_matrix(fValsMat) || ...
                    any(size(fValsMat)~=size(jMargValsDataObj.baseMat))
                errId = ['MAPS:',mfilename('class'),...
                    ':BadUpdateBaseDataInput1'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% SET BASE MATRIX PROPERTY
            jMargValsDataObj.baseMat = fValsMat;
            %% REMERGE MATRIX OF DATA TO COMPUTE NEW JUDGEMENT VALUES
            jMargValsDataObj.mergeOverlappingDataIntoSingleMatrix();
        end        
    end
    %% PROTECTED METHODS
    methods (Access=protected)
        %% METHOD TO MERGE OVERLAPPING DATA INTO SINGLE MATRIX
        function mergeOverlappingDataIntoSingleMatrix(jValsDataObj)
            % This method merges overlapping data into a merged matrix.
            % This method facilitates the user input of values for the same
            % variable in two or more separate operations. It works on the
            % presumption that the input data is valid (which is
            % guaranteed by the base class object). It also converts the
            % marginal judgements information to levels.
            %
            % INPUTS:
            %   -> jValsDataObj: judgement values data object
            %
            % OUTPUTS:
            %   -> none
            %
            %% COMPUTE DIMENSION OF MERGED & UNMERGED MNEMONICS
            njMnems = size(jValsDataObj.mnems,1);
            njUnmergedMnems = size(jValsDataObj.cellArray,1);
            %% MERGE DATA VECTORS WHEN EACH JUDGEMENTS MNEMONIC IS UNIQUE
            if njUnmergedMnems == njMnems
                jMargValsMat = jValsDataObj.uMat;
                jValsMatLogicals = jValsDataObj.uMatLogicals;
            end
            %% MERGE DATA VECTORS IN CASE OF OVERLAPS
            % Starts by setting the data equal to the vector corresponding
            % to the first of the unique variable mnemonics found in the
            % unmerged set. It then merges all other judgement vectors for
            % that variable using the unmerged logiclas from above.
            if njUnmergedMnems > njMnems
                jMargValsMat = NaN*ones(njMnems,jValsDataObj.H);
                for ij = 1:njMnems
                    ijInds = find(ismember(jValsDataObj.cellArray(:,1),...
                        jValsDataObj.mnems{ij}));
                    jMargValsMat(ij,:) = jValsDataObj.uMat(ijInds(1),:);
                    nijInds = size(ijInds,1);
                    for jMerger = 2:nijInds
                        jMergerInd = ijInds(jMerger);
                        jMergerLogicals = ...
                            jValsDataObj.uMatLogicals(jMergerInd,:);
                        jMargValsMat(ij,jMergerLogicals) = ...
                            jValsDataObj.uMat(jMergerInd,jMergerLogicals);
                    end
                end
                jValsMatLogicals = ~isnan(jMargValsMat);
            end
            %% CONVERT MARGINALS TO LEVELS
            jValsMat = NaN*ones(njMnems,jValsDataObj.H);
            jInds = jValsDataObj.inds;
            for ij = 1:njMnems
                ijInd = jInds(ij);
                if jValsDataObj.isLevelsDifference(ijInd)
                    jValsMat(ij,jValsMatLogicals(ij,:)) = jValsDataObj...
                        .baseMat(ijInd,jValsMatLogicals(ij,:))+...
                        jMargValsMat(ij,jValsMatLogicals(ij,:));
                else
                    jValsMat(ij,jValsMatLogicals(ij,:)) = jValsDataObj...
                        .baseMat(ijInd,jValsMatLogicals(ij,:)).*...
                        (1+jMargValsMat(ij,jValsMatLogicals(ij,:))/100);
                end
            end
            %% SET THE MAT PROPERTY OF THE OBJECT & CORRESPONDING LOGICALS
            jValsDataObj.mat = jValsMat;
            jValsDataObj.matLogicals = jValsMatLogicals;
        end
    end
end