classdef Values < Judgements.Data.BaseClass
    % This m-file defines a concrete values judgement data class.
    % This header only defines and discusses aspects of the class that are
    % not included and discussed in the super-class.
    %
    % PUBLIC PROPERTIES:
    %   -> matLogicals: matrix of logicals corresponding to the merged
    %      matrix ("mat" property defined in the super-class) with true
    %      denoting non-NaN data
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> constructor: requires a set of model mnemonics, forecast
    %      horizon scalar and, optionally, isLockedForEditing flag & 
    %      returns an empty judgement data object
    %   -> applyJudgementToForecastDataset: applies the judgement on the
    %      object to a forecast dataset (matrix)
    %
    % DETAILS:
    %   -> The values judgement data class is used as the basis for all
    %      judgements on endogenous variables, shocks and time trends
    %      (since fundamentally these all look the same).
    %   -> This m-file depends on the interface defined in the abstract
    %      super-class. See the super-class header for more details.
    %   -> This sub-class fills in the abstract methods defined in the
    %      super-class that are specific to the values judgement class
    %      (as opposed to the instruments data sub-class).
    %   -> In order to instantiate this sub-class, the constructor must be
    %      called with model mnemonics and forecast horizon scalar inputs.
    %      Optionally, the constructor may also be called with a third,
    %      logical scalar input dictating whether or not the object is
    %      locked for editing. This is useful because not all models
    %      support all types of judgement. These inputs are validated in 
    %      the super-class rather than below (on the DRY principle).
    %   -> It extends the super-class in two ways. First, it computes a
    %      logicals matrix to go with the values matrix. This is useful in
    %      validation both within and outside of this class (i.e. cross-
    %      validation across different judgement components). In the
    %      example described in the super-class, the logicals matrix would
    %      be as follows:
    %                   matLogicals = [true true false false;
    %                                  true false true false]
    %   -> By a similar logic, it also contains a private unmerged matrix
    %      for use within the object. This uMatLogicals property
    %      corresponds to the information in uMat (defined in the base
    %      class) in the same way as matLogicals corresponds to the "mat"
    %      property. In the example described the base class. uMatLogicals
    %      would look as follows:
    %                   uMatLogicals = [true false false false;
    %                                   true false true  false;
    %                                   false true false false];
    %   -> Second, this values data sub-class contains a mthod for actually
    %      implementing the judgements it contains on a matrix of forecast
    %      data. This method is used by the shocks and time-varying trends
    %      instructions judgement classes to update the relevant forecast
    %      run data.
    %
    % NOTES:
    %   -> none
    %
    % This version: 22/11/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (SetAccess=protected)
        matLogicals
    end
    properties (Access=protected)
        uMatLogicals
    end
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR
        function jValsDataObj = Values(varargin)
            % This is the constructor method.
            %
            % INPUTS:
            %   -> varargin: two- or three input call (see super-class
            %      constructor)
            %
            % OUTPUTS:
            %   -> jValsDataObj: instance of the object
            %
            %% CALL THE SUPER-CLASS CONSTRUCTOR
            % Note that input validation is done in the super-class
            % constructor.
            jValsDataObj@Judgements.Data.BaseClass(varargin{:});
        end
        %% APPLY JUDGEMENT TO FORECAST DATASET
        function fValsMatUpdated = ...
                applyJudgementToForecastDataset(jValsDataObj,fValsMat)
            % This method update a forecast dataset with the judgements.
            %
            % INPUTS:
            %   -> jValsDataObj: judgement values data object
            %   -> fValsMat: matrix of forecast data to update
            %
            % OUTPUTS:
            %   -> fValsMatUpdated: forecast data updated for judgements
            %
            %% CHECK INPUT
            % An alternative implementation here would be to restrict
            % access to particular classes.
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),...
                    ':BadApplyJudgementToForecastDatasetNargin'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~is_finite_real_two_dimensional_numeric_matrix(fValsMat) || ...
                    size(fValsMat,1)~=size(jValsDataObj.modelMnems,1) || ...
                    size(fValsMat,2)~=jValsDataObj.H
                errId = ['MAPS:',mfilename('class'),...
                    ':BadApplyJudgementToDatasetInput1'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% INITIALISE OUTPUT
            fValsMatUpdated = fValsMat;
            %% UPDATE FORECAST DATASET USING INDICES, LOGICALS & VALUES
            jInds = jValsDataObj.inds;
            jValsMatLogicals = jValsDataObj.matLogicals;
            jValsMat = jValsDataObj.mat;
            for t = 1:jValsDataObj.horizon
                fValsMatUpdated(jInds(jValsMatLogicals(:,t)),t) = ...
                    jValsMat(jValsMatLogicals(:,t),t);
            end
        end
    end
    %% PROTECTED METHODS
    methods (Access=protected)
        %% METHOD TO INITIALISE MATRIX ON CONSTRUCTION
        function initialiseMatrixOnConstruction(jValsDataObj,H)
            % This method initialises the judgement values matrix.
            %
            % INPUTS:
            %   -> jValsDataObj: judgement values data object
            %   -> H: forecast horizon scalar
            %
            % OUTPUTS:
            %   -> none
            %
            %% INITIALISE MATRIX TO EMPTY NAN ACROSS FORECAST HORIZON
            jValsDataObj.mat = NaN*ones(0,H);
            %% INITIALISE LOGICALS MATRIX ACCORDINGLY
            jValsDataObj.matLogicals = false(0,H);
        end
        %% METHOD TO CONVERT CELL ARRAY OF VALUES VECTORS TO MATRIX
        function convertCellArrayOfVectorsToMatrix(jValsDataObj,jCell)
            % This method converts a cell array of values vectors to a mat.
            % The values matrix created is an unmerged matrix in the sense
            % that vectors corresponding to the same variable are not
            % merged together at this stage.
            %
            % INPUTS:
            %   -> jValsDataObj: judgement values data object
            %   -> jCell: input judgement cell array
            %
            % OUTPUTS:
            %   -> none
            %
            %% USE MAPS HELPER TO COMPUTE THE MATRIX EQUIVALENT OF THE VECS
            % This function returns a matrix of data across the whole
            % forecast horizon, H, with NaNs padding it out if necessary.
            % Note that in the future this helper function may become
            % obsolete (if NLBL judgements are treated as objects as well)
            % in which case the logic should be moved to this function.
            jValsDataObj.uMat = ...
                convert_cell_array_of_vectors_to_matrix_equivalent(...
                jCell(:,2),jValsDataObj.H);
            %% COMPUTE CORRESPONDING UNMERGED MATRIX OF LOGICALS
            jValsDataObj.uMatLogicals = ~isnan(jValsDataObj.uMat);
        end
        %% METHOD TO MERGE OVERLAPPING DATA INTO SINGLE MATRIX
        function mergeOverlappingDataIntoSingleMatrix(jValsDataObj)
            % This method merges overlapping data into a merged matrix.
            % This method facilitates the user input of values for the same
            % variable in two or more separate operations. It works on the
            % presumption that the input data is valid (which is
            % guaranteed by the base class object).
            %
            % INPUTS:
            %   -> jValsDataObj: judgement values data object
            %
            % OUTPUTS:
            %   -> none
            %
            %% COMPUTE DIMENSION OF MERGED MNEMONICS
            njMnems = size(jValsDataObj.mnems,1);
            %% MERGE DATA VECTORS WHEN EACH JUDGEMENTS MNEMONIC IS UNIQUE
            if size(jValsDataObj.cellArray,1) == njMnems
                jValsDataObj.mat = jValsDataObj.uMat;
                jValsDataObj.matLogicals = jValsDataObj.uMatLogicals;
                return
            end
            %% SETUP EMPTY MATRIX USING DIMENSION OF MERGED (UNIQUE) MNEMS
            jValsMat = NaN*ones(njMnems,jValsDataObj.H);
            %% MERGE DATA FOR EACH UNIQUE JUDGEMENT IN TURN
            % Starts by setting the data equal to the vector corresponding
            % to the first of the unique variable mnemonics found in the
            % unmerged set. It then merges all other judgement vectors for
            % that variable using the unmerged logiclas from above.
            for ij = 1:njMnems
                ijInds = find(ismember(jValsDataObj.cellArray(:,1),...
                    jValsDataObj.mnems{ij}));
                jValsMat(ij,:) = jValsDataObj.uMat(ijInds(1),:);
                nijInds = size(ijInds,1);
                for jMerger = 2:nijInds
                    jMergerInd = ijInds(jMerger);
                    jMergerLogicals = ...
                        jValsDataObj.uMatLogicals(jMergerInd,:);
                    jValsMat(ij,jMergerLogicals) = ...
                        jValsDataObj.uMat(jMergerInd,jMergerLogicals);
                end
            end
            %% SET THE MAT PROPERTY OF THE OBJECT & CORRESPONDING LOGICALS
            jValsDataObj.mat = jValsMat;
            jValsDataObj.matLogicals = ~isnan(jValsMat);
        end
        %% METHOD TO COMPUTE HORIZON OF JUDGEMENT VALUES DATA
        function jValsHorizon = computeHorizonOfData(jValsDataObj)
            % This method computes the horizon of the values data.
            %
            % INPUTS:
            %   -> jValsDataObj: judgement values data object
            %
            % OUTPUTS:
            %   -> jValsHorizon: non-negative numeric integer
            %
            %% COMPUTE HORIZON (INCLUDING IN SPECIAL CASE OF EMPTY OBJECT)
            if jValsDataObj.isNonEmpty
                [~,jValsHorizon] = find(jValsDataObj.matLogicals,1,'last');
            else
                jValsHorizon = 0;
            end
        end
        %% METHOD TO VALIDATE NON-OVERLAPS IN INPUT VECTORS
        function validateInputDataIsNonOverlapping(jValsDataObj,jCell)
            % Validates that user input vectors are non-conflicting.
            %
            % INPUTS:
            %   -> jValsDataObj: judgement values data object
            %   -> jCell: input judgement cell array
            %
            % OUTPUTS:
            %   -> none
            %
            %% CONVERT CELL ARRAY OF VECTORS TO A MATRIX
            jValsDataObj.convertCellArrayOfVectorsToMatrix(jCell);
            %% VALIDATE THAT VALUES ARE NON-OVERLAPPING
            % Note that this uses the umerged logicals matrix computed in
            % the "convertCellArrayOfVectorsToMatrix" method.
            errId = ['MAPS:',mfilename('class'),':OverlappingValuesData'];
            Judgements.Data.BaseClass.validateLogicalsAreNonOverlapping(...
                jCell(:,1),jValsDataObj.uMatLogicals,errId);
        end
        %% METHOD TO VALIDATE INPUT DATA VECTORS
        function validateInputDataVectors(jValsDataObj,jCell)
            % This method checks that user input values vectors are valid.
            %
            % INPUTS:
            %   -> jValsDataObj: judgement values data object
            %   -> jCell: input judgement cell array
            %
            % OUTPUTS:
            %   -> none
            %
            %% FIND INDICES OF INPUT VECTORS THAT ARE NOT VALID
            % See the helper below for details of what constitutes valid.
            badVecsLogicals = ~cellfun(...
                @jValsDataObj.isInputVectorValid,jCell(:,2));
            %% THROW AN EXCEPTION AS APPROPRIATE
            if any(badVecsLogicals)
                errId = ['MAPS:',mfilename('class'),':InvalidValuesData'];
                generate_MAPS_exception_add_causes_and_throw(...
                    errId,jCell(:,1),badVecsLogicals);
            end
        end
    end
    %% PRIVATE METHODS
    methods (Access=private)
        %% FUNCTION TO VALIDATE A SINGLE VALUES VECTOR
        function isValid = isInputVectorValid(~,jValsVec)
            % This method validates a single values vector.
            %
            % INPUTS:
            %   -> jValsVec: judgement values data vector
            %
            % OUTPUTS:
            %   -> isValid: true/false
            %
            %% CHECK VALUES VECTOR IS VALID
            % It is valid if: it is a real numeric row vector containing no
            % inf (or -inf) values and at least one of the data points is
            % non-NaN.
            isValid = (...
                is_non_inf_real_numeric_row_vector(jValsVec) && ...
                ~all(isnan(jValsVec)));
        end
    end
end