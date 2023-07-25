classdef (Abstract=true) BaseClass < handle
    % This m-file defines an abstract inversion data class.
    % This class can be made concrete through the sub-classes of
    % Inversion.Data.Indices, Inversion.Data.Values & 
    % Inversion.Data.Weights.
    %
    % PUBLIC PROPERTIES:
    %   -> cellArray: an inversion data cell array - 1*H row vector of
    %      period-wise inversion data column vectors
    %   -> numberPerPeriod: number of data per period
    %   -> isNonEmpty: true/false
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> constructor: requires a forecast horizon integer
    %
    % DETAILS:
    %   -> This m-file defines an abstract interface for concrete indices,
    %      values and weights inversion data sub-classes. These classes are
    %      in turn part of an inversion instructions set. The main purpose
    %      of this design is to avoid repetition of code. All of the 
    %      methods that are specific to the particular type of data in each
    %      sub-class are defined as abstract methods below and implemented
    %      within each of the sub-classes.
    %   -> See each of the sub-classes for more details, but note that the
    %      only difference between them is the type of data that is 
    %      permitted as input (positive, unique integers for indices, 
    %      positive finite real for weights etc).
    %   -> In order to instantiate either of the sub-classes, the relevant
    %      constructor must be called. These in turn call the constructor
    %      in this method. The rest of this header explains the properties
    %      on the object using the following constructor inputs as an
    %      example:
    %                   H = 4;
    %   -> An inversion data cell array (stored in the cellArray property)
    %      is the fundamental property of the inversion data class and can
    %      be set by users directly. For example, given the constructor 
    %      inputs from above, a valid inversion indices data cell array 
    %      could be as follows (see the indices sub-class for details of 
    %      validation):
    %                   iCell ={[1] [2;1]};
    %   -> One function this method performs is to take the cell array
    %      input and extend it so that is covers the whole forecast
    %      horizon (this facilitates performance of the inversion algorithm
    %      itself, which can proceed on the basis that all inversion data 
    %      covers the whole forecast horizon). In this example, the 
    %      extended cell array would look as follows:
    %                   iCell ={[1] [2;1] zeros(0,1) zeros(0,1)};
    %   -> Finally, this class also computes the number of inversion data
    %      points per period whenever the inversion cell array is set. In
    %      the example above that would be:
    %                   nDataPerPeriod = [1 2 0 0];
    %
    % NOTES:
    %   -> none
    %
    % This version: 26/11/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (Dependent=true)
        cellArray
        isNonEmpty
    end
    properties (SetAccess=private)
        numberPerPeriod
    end
    properties (Access=protected)
        pCellArray
        H
    end
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR
        function iDataObj = BaseClass(H)
            % Constructor method for the Inversion.Data base class.
            % It initailises the object into an empty, but valid state.
            %
            % INPUTS:
            %   -> H: forecast horizon scalar
            %
            % OUTPUTS:
            %   -> iDataObj: instance of the object
            %
            %% CHECK INPUTS
            if nargin < 1
                errId = ['MAPS:',mfilename('class'),':BadConstructNargin'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~is_positive_real_numeric_scalar(H)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% SET FORECAST HORIZON PROPERTY
            iDataObj.H = H;     
            %% CREATE EMPTY INVERSION DATA & SET CELL ARRAY PROPERTY
            iDataObj.pCellArray = iDataObj.createEmptyInversionDataCell(H);
            %% SET NUMBER PER PERIOD PROPERTY
            iDataObj.numberPerPeriod = zeros(1,H);
        end
        %% SET METHOD FOR CELL ARRAY
        function set.cellArray(iIndsDataObj,iCell)
            iIndsDataObj.validateInputCellArray(iCell);
            iIndsDataObj.pCellArray = ...
                iIndsDataObj.extendInversionDataCell(iCell);
            iIndsDataObj.numberPerPeriod = cellfun(@(x) size(x,1),...
                iIndsDataObj.pCellArray);
        end
        %% GET METHOD FOR CELL ARRAY
        function iCell = get.cellArray(iDataObj)
            iCell = iDataObj.pCellArray;
        end
        %% IS NON EMPTY GET METHOD
        function isNonEmpty = get.isNonEmpty(iDataObj)
            nPerPeriod = iDataObj.numberPerPeriod;
            if any(nPerPeriod>0)
                isNonEmpty = true;
            else
                isNonEmpty = false;
            end
        end
    end
    %% ABSTRACT METHODS (MUST BE IMPLEMENTED IN THE CONCRETE SUB-CLASSES)
    methods (Abstract=true,Access=protected)
        validateInputCellArrayType(iDataObj,iCell);
    end
    %% PRIVATE METHODS
    methods (Access=private)
        %% AUGMENT INVERSION DATA TO SPAN WHOLE HORIZON
        function iCellExtended = extendInversionDataCell(iDataObj,iCell)
            % Method to extend the inversion data cell array.
            % This ensures that the inversion data covers the whole
            % forecast horizon
            %
            % INPUTS:
            %   -> iCell: inversion data cell
            %
            % OUTPUTS:
            %   -> iCellExtended: inversion data cell extended
            %
            %% COMPUTE SIZE OF INPUT INVERSION CELL
            S = size(iCell,2);
            %% EXTEND THE CELL ARRAY WITH EMPTY DATA
            iCellExtended = [iCell ...
                iDataObj.createEmptyInversionDataCell(iDataObj.H-S)];
        end
        %% CREATE EMPTY INVERSION DATA
        function iDataCellEmpty = createEmptyInversionDataCell(~,H)
            % Method to create an empty inversion data cell array.
            %
            % INPUTS:
            %   -> H: horizon
            %
            % OUTPUTS:
            %   -> iDataCellEmpty: empty inversion data cell
            %
            %% CREATE EMPTY CELL ARRAY
            iDataCellEmpty = cell(1,H);
            %% FILL IT WITH ZEROS
            for t = 1:H
                iDataCellEmpty{t} = zeros(0,1);
            end
        end
        %% FUNCTION TO VALIDATE INVERSION DATA CELL
        function validateInputCellArray(iDataObj,iCell)
            % Method to validate an input cell array
            %
            % INPUTS:
            %   -> iCell: inversion data cell
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK THAT INPUT IS A ROW CELL ARRAY & IS NOT LONGER THAN H
            if ~is_row_cell_array(iCell) || size(iCell,2)>iDataObj.H
                errId = ['MAPS:',mfilename('class'),':BadInversionData'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CHECK SPECIFIC INVERSION DATA TYPE (IMPLEMENT IN SUB-CLASS)
            iDataObj.validateInputCellArrayType(iCell);
        end
    end
end