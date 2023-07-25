classdef Indices < Inversion.Data.BaseClass
    % This m-file defines a concrete inversion indices data class.
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
    %   -> constructor: requires a forecast horizon scalar and a
    %      maximum index number as input and returns an empty indices
    %      object
    %
    % DETAILS:
    %   -> This m-file depends on the interface defined in the abstract
    %      super-class. See the super-class header for more details.
    %   -> This sub-class fills in the abstract validation method defined 
    %      in the super-class that is specific to validation of inversion
    %      indices data.
    %   -> In order to instantiate this sub-class, the constructor must be
    %      called with a forecast horizon scalar and maximum index number 
    %      inputs. The forecast horizon input is validated in the super-
    %      class rather than below (on the DRY principle). The maximum
    %      index number represents the total number of variables or shocks
    %      (depending on the use case) in the model. It is validated and 
    %      stored in this sub-class and is used in validation of input 
    %      indices data. For example, it is is not possible to set
    %      inversion data for the 4th variable in a model that only has 3
    %      variables (in which case maxIndexNumber=3).
    %   -> Note that the example in the super-class header applies directly
    %      directly to inversion indices data.
    %
    % NOTES:
    %   -> none
    %
    % This version: 26/11/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties (Access=protected)
        maxIndexNumber
    end
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR
        function iIndsDataObj = Indices(H,maxIndexNumber)
            % Constructor method for the Inversion.Data.Indices class.
            % It initialises the object into an empty, but valid state.
            %
            % INPUTS:
            %   -> H: forecast horizon scalar
            %   -> maxIndexNumber: maximum permitted index number (i.e.
            %      total number variables or shocks (depending on use case)
            %      in the model) 
            %
            % OUTPUTS:
            %   -> iIndsDataObj: instance of the object
            %
            %% CHECK NUMBER OF INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadConstructNargin'];
                generate_and_throw_MAPS_exception(...
                    errId,{num2str(nargin)});
            end
            %% INITIALISE PROPERTIES DEFINED IN SUPER-CLASS
            iIndsDataObj@Inversion.Data.BaseClass(H);
            %% CHECK MAX INDEX NUMBER INPUT
            if ~is_positive_real_numeric_scalar(maxIndexNumber)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput2'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% SET MAX INDEX NUMBER PROPERTY
            iIndsDataObj.maxIndexNumber = maxIndexNumber;
        end
    end
    %% PROTECTED METHODS
    methods (Access=protected)
        %% VALIDATION METHOD FOR INVERSION INDICES DATA
        function validateInputCellArrayType(iIndsDataObj,iCell)
            % Method to validate an indices inversion data cell array.
            %
            % INPUTS:
            %   -> iCell: inversion data cell
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK INDICES DATA
            % Indices inversion data must be column vectors of unique
            % indices that do not exceed the maximum index number.
            if any(~cellfun(@(x) ...
                    is_column_vector_of_unique_indices_in_range(...
                    x,iIndsDataObj.maxIndexNumber),iCell))
                errId = ['MAPS:',mfilename('class'),':BadInversionData'];
                generate_and_throw_MAPS_exception(errId);
            end
        end
    end
end