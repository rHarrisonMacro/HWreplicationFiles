classdef (Abstract=true) BaseClass < handle
    % This m-file defines an abstract judgement data class as an interface.
    % This class can be made concrete through the Values and Instruments
    % sub-classes. It is part of a package of classes that together form
    % part of a MAPS LSS model judgements instructions object.
    %
    % PUBLIC PROPERTIES:
    %   -> cellArray: two-column judgement data cell array with mnemonics
    %      in the first column and row vectors of data in the second column
    %   -> mnems: mnemonics corresponding to the judgement data
    %   -> mat: matrix of merged judgement data (with each row
    %      corresponding to each of the mnemonics)
    %   -> isNonEmpty: true if there is any judgement data (false
    %      otherwise)
    %   -> inds: index numbers corresponding to the position of each
    %      mnemonic in the model
    %   -> horizon: maximum horizon of judgement data
    %   -> isLockedForEditing: true/false on construction
    %
    % EVENTS:
    %   -> jDataObjStateChanged: broadcast on changes in state
    %
    % PUBLIC METHODS:
    %   -> constructor: requires a set of model mnemonics, forecast
    %      horizon scalar and, optionally, isLockedForEditing flag & 
    %      returns an empty judgement data object
    %   -> add: allows for judgements to be added one-by-one (requires 2
    %      inputs) or in bulk (requires 1 input: a judgement cell array)
    %   -> overwrite: allows for all judgement data to be overwritten with
    %      a new set
    %   -> remove: allows for judgements to be removed
    %
    % DETAILS:
    %   -> This m-file defines an abstract interface for concrete values
    %      judgement data and instruments judgement data sub-classes.
    %      These classes are in turn used to specify judgement values
    %      (either for shocks or variables) or to specify instruments (in
    %      (conditioning a projection). The main purpose of this setup is
    %      to avoid repetition of code. All of the methods that are 
    %      specific to the particular type of data in each sub-class are 
    %      defined as abstract methods below and implemented within each of
    %      the sub-classes.
    %   -> See each of the sub-classes for more details, but note that the
    %      main difference between them is that values judgements must be
    %      non-infinite real numeric data (with periods in which they are
    %      not being applied specified as NaNs) and that instruments data
    %      is logicals or equivalent numeric 0-1 data.
    %   -> In order to instantiate either of the sub-classes, the relevant
    %      constructor must be called with model mnemonics and forecast
    %      horizon scalar inputs. These in turn call the constructor in
    %      this method, which stores the inputs as private properties on
    %      the model and used for validation. The forecast horizon input
    %      is also used for construction of the "mat" property (see below).
    %      The rest of this header explains the properties on the object
    %      and its public methods using the following constructor inputs as
    %      an example:
    %                   modelMnems = {'b';'c';'a'};
    %                   H = 4;
    %      Note that, optionally, the construction of the object may
    %      include an "isLockedForEditing" flag, which will not allow users
    %      to edit the object once instantiated, meaning that it will
    %      remain in an empty state. This is useful in building up a
    %      complete set of judgement data using models that may not be
    %      compatible with all possible judgement data components.
    %   -> A judgement data cell array (stored in the cellArray property)
    %      is the fundamental part of a judgement data object. For example,
    %      given the constructor inputs from above, a valid judgement
    %      values data cell array could be as follows (see the Instruments
    %      sub-class for a very brief discussion of the difference):
    %                   jCell ={'a' 3;
    %                           'b' [1 NaN 3];
    %                           'a' [NaN 2]};
    %   -> Any attempt to pass in invalid data (either with mnemonics that
    %      don't exist in the set on construction or with vectors that
    %      exceed the forecast horizon in length or with invalid data given
    %      the sub-class) will be met with an exception and the object will
    %      remain in the same state. In addition, any attempt to edit data
    %      using any of the methods descibed below will be met with an
    %      exception in the event that the object "isLockedForEditing".
    %   -> There are two ways of constructing a cell array like this.
    %      The first is to input the cell array using the "overwrite"
    %      method:
    %                   jDataObj.overwrite(jCell);
    %      Note that if the object were in an empty state to begin with (eg
    %      if it had just been instantiated), then this would be equivalent
    %      to:
    %                   jDataObj.add(jCell);
    %   -> It is also possible to use the add method to build up the cell
    %      array in bits. For example, the first row of the cell array
    %      could be added as follows:
    %                   jDataObj.add('a',3);
    %      It would then be straightforward to add the 2nd and 3rd rows in
    %      the same way (either in one go as a cell array or one at a time
    %      with single inputs for the mnemonics and vectors).
    %   -> It is also possible to remove judgements (eg if a mistake has
    %      been made). For example, the following command would remove the
    %      'b' judgement:
    %                   jDataObj.remove('b');
    %      Note that it is only possible to remove all judgement values
    %      associated with a particular mnemonic (it would be possible
    %      to cater for the removal of one or more data points in the
    %      future by extending the remove method to allow for an optional
    %      second input of logicals with "true" corresponding to periods in
    %      which the judgement data should be removed).
    %   -> Notice that the judgement on 'a' appears in two rows in the
    %      example above. This is valid because the second row includes a
    %      NaN element at the start and so does not clash with the first
    %      judgement input. These judgements are merged together in the
    %      "mnems" and "mat" properties. Given the example judgement cell
    %      above and a forecast horizon input on construction of H=4, mnems
    %      and mat would look as follows:
    %                   mnems = {'a';'b'};
    %                   mat = [3  2  NaN NaN;
    %                          1 NaN  3  NaN];
    %   -> The protected property, uMat, is used as an intermediate
    %      variable and contains an "unmerged" matrix. In our example, it
    %      would look as follows, where each row corresponds to the same
    %      row in the cell array:
    %                   uMat = [3   NaN NaN NaN;
    %                           1   NaN  3  NaN;
    %                          NaN   2  NaN NaN];
    %   -> Finally, note that if the model mnemonics input on construction
    %      is as in the example above, then the dependent properties would
    %      be:
    %                   isNonEmpty = true;
    %                   inds = [3;1];
    %                   horizon = 3;
    %
    % NOTES:
    %   -> The class below specifies a number of abstract methods that must
    %      be implemented in each of the sub-classes. It also contains some
    %      protected methods, mainly used in validation.
    %   -> The event broadcast is designed to facilitate validation of an
    %      entire judgements object - see other classes in the +Judgements
    %      package for details of their use.
    %
    % This version: 09/12/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (SetAccess=protected)
        cellArray
        mnems
        mat
        isLockedForEditing
    end
    properties (Dependent=true)
        isNonEmpty
        inds
        horizon
    end
    properties (Access=protected)
        uMat
    end
    properties (SetAccess=private,GetAccess=protected)
        H
        modelMnems
    end
    %% EVENTS
    events
        jDataObjStateChanged
    end
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR METHOD
        function jDataObj = BaseClass(modelMnems,H,isLockedForEditing)
            % Constructor method for the Judgements.Data base class.
            % It initailises the object into an empty, but valid state.
            %
            % INPUTS:
            %   -> modelMnems: column array of mnemonic strings
            %   -> H: forecast horizon scalar
            %   -> isLockedForEditing (optional) true/false
            %
            % OUTPUTS:
            %   -> jDataObj: instance of the object
            %
            %% CHECK INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadConstructNargin'];
                generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
            elseif ~is_column_cell_string_array(modelMnems)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput1'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~is_positive_real_integer(H)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput2'];
                generate_and_throw_MAPS_exception(errId);
            elseif nargin>2 && ~is_logical_scalar(isLockedForEditing)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput3'];
                generate_and_throw_MAPS_exception(errId);                
            end
            %% HANDLE OPTIONAL EDITING LOCK INPUT
            if nargin < 3
                isLockedForEditing = false;
            end
            %% INITIALISE ALL PROPERTIES REQUIRED FOR OBJECT TO BE VALID
            jDataObj.modelMnems = modelMnems;
            jDataObj.H = H;
            jDataObj.cellArray = cell(0,2);
            jDataObj.mnems = cell(0,1);
            jDataObj.initialiseMatrixOnConstruction(H);
            jDataObj.isLockedForEditing = isLockedForEditing;
        end
        %% ADD METHOD
        function add(jDataObj,input1,input2)
            % The add methods allows for data to be added to the object.
            %
            % INPUTS:
            %   -> input1: either a judgement cell array of 1 input is
            %      passed in or a mnemonic for the judgement of 2
            %   -> input2: a judgement row vector of values/logicals if 2
            %      inputs are passed in
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK IF EDITING IS PERMITTED
            if jDataObj.isLockedForEditing
                errId = ['MAPS:',mfilename('class'),':BadEdit'];
                generate_and_throw_MAPS_exception(errId);                
            end
            %% CHECK NUMBER OF INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadAddNargin'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CREATE FLAG FOR THE TWO ALTERNATIVE CALL CASES
            if nargin < 3
                isSingleInputCase = true;
            else
                isSingleInputCase = false;
            end
            %% HANDLE SINGLE INPUT CASE
            if isSingleInputCase
                jCell = input1;
            end
            %% HANDLE DOUBLE INPUT CASE
            if ~isSingleInputCase
                jCell = {input1 input2};
            end
            %% VALIDATE THE ADDITIONAL JUDGEMENT DATA
            try
                jDataObj.validateInputCellArray(jCell);
            catch ValidationE
                if isSingleInputCase
                    errId = [...
                        'MAPS:',mfilename('class'),':BadAddSingleInput'];
                else
                    errId = [...
                        'MAPS:',mfilename('class'),':BadAddDoubleInput'];
                end
                generate_MAPS_exception_add_cause_and_throw(...
                    ValidationE,errId);
            end
            %% VALIDATE CONFLICTS/OVERLAPS WITH EXISTING JUDGEMENT DATA
            jCellUpdated = [jDataObj.cellArray;jCell];
            jDataObj.validateInputDataIsNonOverlapping(jCellUpdated);
            %% UPDATE JUDGEMENT CELL
            jDataObj.cellArray = jCellUpdated;
            %% COMPLETE POST-SET TASKS
            % The judgement mnemonics and matrix are created in the merge
            % method and then an event is broadcast that the state of the
            % object has changed.
            jDataObj.merge();
            notify(jDataObj,'jDataObjStateChanged');
        end
        %% OVERWRITE METHOD
        function overwrite(jDataObj,jCell)
            % The overwrite methods overwrites existing data in the object.
            %
            % INPUTS:
            %   -> jCell: a judgement cell array - see the class header for
            %      more details
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK IF EDITING IS PERMITTED
            if jDataObj.isLockedForEditing
                errId = ['MAPS:',mfilename('class'),':BadEdit'];
                generate_and_throw_MAPS_exception(errId);                
            end
            %% CHECK NUMBER OF INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadOverwriteNargin'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% VALIDATE CELL ARRAY INPUT
            try
                jDataObj.validateInputCellArray(jCell);
            catch ValidationE
                errId = ['MAPS:',mfilename('class'),':BadOverwriteInput'];
                generate_MAPS_exception_add_cause_and_throw(...
                    ValidationE,errId);
            end
            %% UPDATE CELL ARRAY PROPERTY
            jDataObj.cellArray = jCell;
            %% COMPLETE POST-SET TASKS
            % The judgement mnemonics and matrix are created in the merge
            % method and event is broadcast that the state of the object
            % has changed.
            jDataObj.merge();
            notify(jDataObj,'jDataObjStateChanged');
        end
        %% REMOVE METHOD
        function remove(jDataObj,jMnems)
            % The remove methods allows data to be removed from the object.
            %
            % INPUTS:
            %   -> jMnems: mnemonics of the judgements to be removed
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK IF EDITING IS PERMITTED
            if jDataObj.isLockedForEditing
                errId = ['MAPS:',mfilename('class'),':BadEdit'];
                generate_and_throw_MAPS_exception(errId);                
            end
            %% CHECK INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadRemoveNargin'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~is_string_or_column_cell_string_array(jMnems)
                errId = ['MAPS:',mfilename('class'),':BadRemoveInput1'];
                generate_and_throw_MAPS_exception(errId);
            elseif size(unique(jMnems),1) ~= size(jMnems,1)
                errId = ['MAPS:',mfilename('class'),':OddRemoveCommand'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CHECK THAT DATA OBJECT IS NON-EMPTY
            if ~jDataObj.isNonEmpty
                errId = [...
                    'MAPS:',mfilename('class'),':InvalidRemoveCommand'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CHECK THAT ALL INPUT MNEMONICS EXIST IN THE OBJECT
            errId = ['MAPS:',mfilename('class'),':InvalidRemoveMnems'];
            validate_strings_belong_to_reference_set(...
                jMnems,jDataObj.mnems,errId);
            %% REMOVE SPECIFIED JUDGEMENTS & UPDATE CELL ARRAY PROPERTY
            jToRemoveLogicals = ismember(jDataObj.cellArray(:,1),jMnems);
            jCellUpdated = jDataObj.cellArray;
            jCellUpdated(jToRemoveLogicals,:) = [];
            jDataObj.cellArray = jCellUpdated;
            %% UPDATE PRIVATE UNMERGED MATRIX
            % This is required here (and was not in the add and overwrite
            % methods) because it is not necessary to validate the object
            % after removal - if it was valid before, then it must still be
            % valid.
            jDataObj.convertCellArrayOfVectorsToMatrix(jCellUpdated);
            %% COMPLETE POST-SET TASKS
            % Note that this has to deal with the case in which all
            % existing judgements have been removed in which case the
            % mnemonic and matrix properties must be reinitialised.
            if jDataObj.isNonEmpty
                jDataObj.merge();
            else
                jDataObj.mnems = cell(0,1);
                jDataObj.initialiseMatrixOnConstruction(jDataObj.H);
            end
            notify(jDataObj,'jDataObjStateChanged');
        end
        %% GET METHOD FOR NON EMPTY
        function jIsNonEmpty = get.isNonEmpty(jDataObj)
            if size(jDataObj.cellArray,1) > 0
                jIsNonEmpty = true;
            else
                jIsNonEmpty = false;
            end
        end
        %% GET METHOD FOR INDICES
        function jInds = get.inds(jDataObj)
            jInds = lookup_index_numbers_in_string_array(...
                jDataObj.modelMnems,jDataObj.mnems,true);
        end
        %% GET METHOD FOR HORIZON
        function jHorizon = get.horizon(jDataObj)
            jHorizon = jDataObj.computeHorizonOfData();
        end
    end
    %% PROTECTED ACCESS METHODS
    methods (Access=protected)
        %% MERGE METHOD
        function merge(jDataObj)
            % Merges the judgement mnemonics & data vectors into a matrix.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> none
            %
            %% MERGE MNEMONICS
            jDataObj.mnems = unique(jDataObj.cellArray(:,1),'stable');
            %% CALL SUB-CLASS METHOD TO MERGE DATA (DEPENDENT ON TYPE)
            jDataObj.mergeOverlappingDataIntoSingleMatrix();
        end
        %% VALIDATION METHOD FOR INPUT CELL ARRAY
        function validateInputCellArray(jDataObj,jCell)
            % Validates cell arays input through add & overwrite methods.
            %
            % INPUTS:
            %   -> jCell: judgement cell array input
            %
            % OUTPUTS:
            %   -> none
            %
            %% VALIDATE THAT JUDGEMENT CELL IS INDEED A 2-COLUMN CELL
            if ~is_two_dimensional_cell_array(jCell) || size(jCell,2)~=2
                errId = ['MAPS:',mfilename('class'),':BadJudgementData'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% VALIDATE FIRST & SECOND COLUMNS ARE STRINGS & ROW VECS
            % Note that the row vectors must be shorter than the forecast
            % horizon, H, in length.
            if ~is_column_cell_string_array(jCell(:,1)) || ...
                    any(cellfun(@ischar,jCell(:,2))) || ...
                    ~all(cellfun(@is_row_array,jCell(:,2))) || ...
                    ~all(cellfun(@(x) size(x,2),jCell(:,2))<=jDataObj.H)
                errId = [...
                    'MAPS:',mfilename('class'),':BadJudgementDataContent'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% VALIDATE JUDGEMENT MNEMONICS FORM PART OF MODEL SET
            errId = ['MAPS:',mfilename('class'),':BadJudgementMnems'];
            validate_strings_belong_to_reference_set(...
                jCell(:,1),jDataObj.modelMnems,errId);
            %% CALL SUB-CLASS METHOD TO VALIDATE CONTENT OF DATA VECTORS
            jDataObj.validateInputDataVectors(jCell);
            %% CALL SUB-CLASS METHOD TO VALIDATE DATA VECS DO NOT OVERLAP
            % This is necessary to ensure that input data is not
            % conflicting (eg an attempt to set values judgement data twice
            % or more for the same variable in the same period).
            jDataObj.validateInputDataIsNonOverlapping(jCell);
        end
    end
    %% ABSTRACT METHODS (MUST BE IMPLEMENTED IN THE CONCRETE SUB-CLASSES)
    methods (Abstract=true,Access=protected)
        initialiseMatrixOnConstruction(jDataObj,H);
        convertCellArrayOfVectorsToMatrix(jDataObj,jCell);
        mergeOverlappingDataIntoSingleMatrix(jDataObj);
        computeHorizonOfData(jDataObj);
        validateInputDataVectors(jDataObj,jCell);
        validateInputDataIsNonOverlapping(jDataObj,jCell);
    end
    %% STATIC METHODS WITH RESTRICTED ACCESS
    methods (Static=true,...
            Access={?Judgements.Data.BaseClass,?Judgements.Instructions})
        %% FUNCTION TO VALIDATE THAT LOGICALS ARE NON-OVERLAPPING
        function validateLogicalsAreNonOverlapping(...
                jMnems,jMatLogicals,errId)
            % Validates a set of (judgement) logicals is non-overlapping.
            % Logicals are overlapping if they are associated with the same
            % mnemoic and are equal true in one or more of the same
            % periods.
            %
            % INPUTS:
            %   -> jMnems: nj*1 cell string array of judgement mnemonics
            %   -> jMatLogicals: nj*H matrix of logicals
            %   -> errId: exception identifier
            %
            % OUTPUTS:
            %   -> none
            %
            %% FIND REPEATED MNEMONICS (PRE-REQUISITE FOR OVERLAPS)
            jRepeatedMnems = find_repeated_strings(jMnems);
            %% STOP SEARCH IF THERE ARE NO REPEATS
            if isempty(jRepeatedMnems)
                return
            end
            %% COUNT REPEATS & SEARCH FOR OVERLAPS
            % An overlap is defined as two or more "true" values occurring
            % for the same judgement in one or more time periods (where
            % true signifies that there is data in that period either shock
            % usage / instruments or judgement values).
            nRepeatedMnems = size(jRepeatedMnems,1);
            isjRepeatedOverlapping = false(nRepeatedMnems,1);
            for ijRepeated = 1:nRepeatedMnems
                ijRepeatedInds = ismember(jMnems,jRepeatedMnems);
                if any(sum(jMatLogicals(ijRepeatedInds,:))>1)
                    isjRepeatedOverlapping(ijRepeated) = true;
                end
            end
            %% THROW EXCEPTION IF ANY OVERLPAS WERE FOUND
            if any(isjRepeatedOverlapping)
                generate_MAPS_exception_add_causes_and_throw(...
                    errId,jRepeatedMnems,isjRepeatedOverlapping);
            end
        end
    end
end