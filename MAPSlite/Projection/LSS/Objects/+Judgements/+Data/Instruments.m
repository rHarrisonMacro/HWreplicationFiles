classdef Instruments < Judgements.Data.BaseClass
    % This m-file defines a concrete judgement instruments data class.
    % This header only defines and discusses aspects of the class that are
    % not included and discussed in the super-class.
    %
    % PUBLIC PROPERTIES:
    %   -> none
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> constructor: requires a set of model mnemonics, forecast
    %      horizon scalar and, optionally, isLockedForEditing flag & 
    %      returns an empty judgement data object
    %
    % DETAILS:
    %   -> The judgement instruments data class is used as the basis for
    %      defining conditioning/inversion judgement information.
    %   -> This m-file depends on the interface defined in the abstract
    %      super-class. See the super-class header for more details.
    %   -> This sub-class fills in the abstract methods defined in the
    %      super-class that are specific to the instruments judgement data
    %      class (as opposed to the values data sub-class).
    %   -> In order to instantiate this sub-class, the constructor must be
    %      called with model mnemonics and forecast horizon scalar inputs.
    %      Optionally, the constructor may also be called with a third,
    %      logical scalar input dictating whether or not the object is
    %      locked for editing. This is useful because not all models
    %      support all types of judgement. These inputs are validated in 
    %      the super-class rather than below (on the DRY principle).
    %   -> Note that the example in the super-class header applies almost
    %      directly to instruments with the exception that inputs must be
    %      either logicals or 0-1 numerics (where 0 or false corresponds to
    %      periods in which that particular instrument is not being used).
    %      For example, a valid instrument judgement cell array (similar 
    %      to the values judgement data used as an example in the super-
    %      class) would be:
    %                   jCell ={'a' true;
    %                           'b' [1 0 1];
    %                           'a' [false true]};
    %   -> In producing a merged matrix, all input numeric 0-1 data is
    %      converted to logicals such that the "mat" property is comprised
    %      of false-true logcials. In the example above it would be:
    %                   mat = [true true false false;
    %                          true false true false];
    %   -> All of the other properties would be identical to those
    %      described in the example in the base class.
    %
    % NOTES:
    %   -> none
    %
    % This version: 25/11/2013
    % Author(s): Matt Waldron
    
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR METHOD
        function jInstrumentsDataObj = Instruments(varargin)
            % This is the constructor method.
            %
            % INPUTS:
            %   -> varargin: two- or three input call (see super-class
            %      constructor)
            %
            % OUTPUTS:
            %   -> jInstrumentsDataObj: instance of the object
            %
            %% CALL THE SUPER-CLASS CONSTRUCTOR
            % Note that input validation is done in the super-class
            % constructor.
            jInstrumentsDataObj@Judgements.Data.BaseClass(varargin{:});
        end
    end
    %% PROTECTED ACCESS METHODS
    methods (Access=protected)
        %% METHOD TO INITIALISE MATRIX ON CONSTRUCTION
        function initialiseMatrixOnConstruction(jInstrumentsDataObj,H)
            % This method initialises the instruments data matrix.
            %
            % INPUTS:
            %   -> jInstrumentsDataObj: judgement shock usages data object
            %   -> H: forecast horizon scalar
            %
            % OUTPUTS:
            %   -> none
            %
            %% INITIALISE MATRIX TO EMPTY FALSE ACROSS FORECAST HORIZON
            jInstrumentsDataObj.mat = false(0,H);
        end
        %% FUNCTION TO CONVERT USAGES VECTORS TO A MATRIX
        function convertCellArrayOfVectorsToMatrix(...
                jInstrumentsDataObj,jCell)
            % This method converts an instrument data cell array to a mat.
            % The logical matrix created is an unmerged matrix in the sense
            % that instruments for the same shock are not merged together
            % at this stage.
            %
            % INPUTS:
            %   -> jInstrumentsDataObj: judgement shock usages data object
            %   -> jCell: input judgement cell array
            %
            % OUTPUTS:
            %   -> none
            %
            %% EXTRACT INSTRUMENTS VECS & COMPUTE NUMBER OF COLUMNS IN EACH
            jInstrumentsVecs = jCell(:,2);
            Sjs = cellfun(@(x) size(x,2),jInstrumentsVecs);
            %% SET LOGICAL MATRIX FOR THE FAST CASE
            % If all the vectors have the same number of data points equal
            % to the forecast horizon, H, then it is possible to convert to
            % a logical matrix directly and more quickly.
            if all(Sjs==jInstrumentsDataObj.H)
                jInstrumentsDataObj.uMat = ...
                    logical(cell2mat(jInstrumentsVecs));
                return
            end
            %% SET LOGICALS MATRIX FOR MORE EXOTIC CASES
            % Note that the matrix is set vector-by-vector up to the
            % maximum number of data points in that vector.
            njUsages = size(jInstrumentsVecs,1);
            jInstrumentsMat = false(njUsages,jInstrumentsDataObj.H);
            for ij = 1:njUsages
                jInstrumentsMat(ij,1:Sjs(ij)) = ...
                    logical(jInstrumentsVecs{ij});
            end
            jInstrumentsDataObj.uMat = jInstrumentsMat;
        end
        %% METHOD TO MERGE OVERLAPPING DATA INTO SINGLE MATRIX
        function mergeOverlappingDataIntoSingleMatrix(jInstrumentsDataObj)
            % This method merges overlapping data into a merged matrix.
            % This method facilitates the user input of instruments for the
            % same shock in two or more separate operations. It works on
            % the presumption that the input data is valid (which is
            % guaranteed by the base class object).
            %
            % INPUTS:
            %   -> jInstrumentsDataObj: judgement instruments data object
            %
            % OUTPUTS:
            %   -> none
            %
            %% COMPUTE DIMENSION OF MERGED MNEMONICS
            njMnems = size(jInstrumentsDataObj.mnems,1);
            %% MERGE DATA VECTORS WHEN EACH JUDGEMENTS MNEMONIC IS UNIQUE
            if size(jInstrumentsDataObj.cellArray,1) == njMnems
                jInstrumentsDataObj.mat = jInstrumentsDataObj.uMat;
                return
            end
            %% SETUP EMPTY MATRIX USING DIMENSION OF MERGED (UNIQUE) MNEMS
            jInstrumentsMat = false(njMnems,jInstrumentsDataObj.H);
            %% MERGE DATA FOR EACH UNIQUE INSTRUMENT JUDGEMENT IN TURN
            % Starts by setting the data equal to the vector corresponding
            % to the first of the unique shock mnemonics found in the
            % unmerged set. It then merges all other vectors for that shock
            % usage into the first vector using an or, |, rule.
            for ij = 1:njMnems
                ijInds = find(ismember(...
                    jInstrumentsDataObj.cellArray(:,1),...
                    jInstrumentsDataObj.mnems{ij}));
                jInstrumentsMat(ij,:) = ...
                    jInstrumentsDataObj.uMat(ijInds(1),:);
                nijInds = size(ijInds,1);
                for jMerger = 2:nijInds
                    jMergerInd = ijInds(jMerger);
                    jInstrumentsMat(ij,:) = (...
                        jInstrumentsMat(ij,:)|...
                        jInstrumentsDataObj.uMat(jMergerInd,:));
                end
            end
            %% SET THE MAT PROPERTY OF THE OBJECT
            jInstrumentsDataObj.mat = jInstrumentsMat;
        end
        %% METHOD TO COMPUTE HORIZON OF USAGES DATA
        function jInstrumentsHorizon = ...
                computeHorizonOfData(jInstrumentsDataObj)
            % This method computes the horizon of the instruments data.
            %
            % INPUTS:
            %   -> jInstrumentsDataObj: judgement shock usages data object
            %
            % OUTPUTS:
            %   -> jInstrumentsHorizon: non-negative numeric integer
            %
            %% COMPUTE HORIZON (INCLUDING IN SPECIAL CASE OF EMPTY OBJECT)
            if jInstrumentsDataObj.isNonEmpty
                [~,jInstrumentsHorizon] = ...
                    find(jInstrumentsDataObj.mat,1,'last');
            else
                jInstrumentsHorizon = 0;
            end
        end
        %% METHOD TO VALIDATE NON-OVERLAPS IN INPUT VECTORS
        function validateInputDataIsNonOverlapping(...
                jInstrumentsDataObj,jCell)
            % Validates that user input vectors are non-conflicting.
            %
            % INPUTS:
            %   -> jInstrumentsDataObj: judgement instruments data object
            %   -> jCell: input judgement cell array
            %
            % OUTPUTS:
            %   -> none
            %
            %% CONVERT CELL ARRAY OF VECTORS TO A MATRIX
            jInstrumentsDataObj.convertCellArrayOfVectorsToMatrix(jCell);
            %% VALIDATE THAT LOGICALS ARE NON-OVERLAPPING
            errId = [...
                'MAPS:',mfilename('class'),':OverlappingInstrumentsData'];
            Judgements.Data.BaseClass.validateLogicalsAreNonOverlapping(...
                jCell(:,1),jInstrumentsDataObj.uMat,errId);
        end
        %% METHOD TO VALIDATE INPUT INSTRUMENTS VECTORS
        function validateInputDataVectors(jInstrumentsDataObj,jCell)
            % This method checks that user input instrument vecs are valid.
            %
            % INPUTS:
            %   -> jInstrumentsDataObj: judgement instruments data object
            %   -> jCell: input judgement cell array
            %
            % OUTPUTS:
            %   -> none
            %
            %% FIND INDICES OF INPUT VECTORS THAT ARE NOT VALID
            % See the helper below for details of what constitutes valid.
            badVecsLogicals = ~cellfun(...
                @jInstrumentsDataObj.isInputVectorValid,jCell(:,2));
            %% THROW AN EXCEPTION AS APPROPRIATE
            if any(badVecsLogicals)
                errId = [...
                    'MAPS:',mfilename('class'),':InvalidInstrumentsData'];
                generate_MAPS_exception_add_causes_and_throw(...
                    errId,jCell(:,1),badVecsLogicals);
            end
        end
    end
    %% PRIVATE METHODS
    methods (Access=private)
        %% FUNCTION TO VALIDATE A SINGLE INSTRUMENTS VECTOR
        function isValid = isInputVectorValid(~,jInstrumentsVec)
            % This method validates a single usages vector.
            %
            % INPUTS:
            %   -> jInstrumentsVec: single shock usages vector
            %
            % OUTPUTS:
            %   -> isValid: true/false
            %
            %% CHECK INSTRUMENTS VECTOR IS VALID
            % It is valid if either: a) it is a logical row vector,
            % containing at least one true value; b) it is a numeric row
            % vector of 0-1 values containing at least one non-zero values.
            isValid = (...
                (is_logical_row_vector(jInstrumentsVec)&&...
                any(jInstrumentsVec)) || ...
                (is_finite_real_numeric_row_vector(jInstrumentsVec)&&...
                all(jInstrumentsVec==0|jInstrumentsVec==1)&&...
                ~all(jInstrumentsVec==0)));
        end
    end
end