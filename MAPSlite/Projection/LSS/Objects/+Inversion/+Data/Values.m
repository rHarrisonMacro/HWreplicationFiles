classdef Values < Inversion.Data.BaseClass
    % This m-file defines a concrete inversion values data class.
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
    %   -> constructor: requires a forecast horizon scalar and returns an 
    %      empty values inversion data object
    %
    % DETAILS:
    %   -> This m-file depends on the interface defined in the abstract
    %      super-class. See the super-class header for more details.
    %   -> This sub-class fills in the abstract validation method defined 
    %      in the super-class that is specific to validation of inversion
    %      values data.
    %   -> In order to instantiate this sub-class, the constructor must be
    %      called with a forecast horizon scalar. The forecast horizon 
    %      input is validated  and the object is initialised in the super-
    %      class rather than below (on the DRY principle).
    %   -> Note that the example in the super-class header applies almost 
    %      directly to inversion values data (with the exception that 
    %      inversion values data can be column vectors of finite, real 
    %      data (rather than positive integers) - see below for details of
    %      the validation).
    %
    % NOTES:
    %   -> none
    %
    % This version: 26/11/2013
    % Author(s): Matt Waldron
    
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR
        function iValsDataObj = Values(varargin)
            % Constructor method for the Inversion.Data.Values class.
            % It initialises the object into an empty, but valid state.
            %
            % INPUTS:
            %   -> H: forecast horizon scalar
            %
            % OUTPUTS:
            %   -> iValsDataObj: instance of the object
            %
            %% CALL THE SUPER-CLASS CONSTRUCTOR
            % Note that input validation is done in the super-class
            % constructor.
            iValsDataObj@Inversion.Data.BaseClass(varargin{:});
        end
    end
    %% PROTECTED METHODS
    methods (Access=protected)
        %% VALIDATION METHOD FOR INVERSION VALUES DATA
        function validateInputCellArrayType(~,iCell)
            % Method to validate a values inversion data cell array.
            %
            % INPUTS:
            %   -> iCell: inversion data cell
            %
            % OUTPUTS:
            %   -> none
            %
            %% CHECK VALUES DATA
            % Values inversion data must be column vectors of finite, real
            % numeric data.
            if any(~cellfun(@is_finite_real_numeric_column_vector,iCell))
                errId = ['MAPS:',mfilename('class'),':BadInversionData'];
                generate_and_throw_MAPS_exception(errId);
            end
        end
    end
end