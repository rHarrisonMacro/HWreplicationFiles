classdef Options < Inversion.Options
    % This m-file defines a conditioning judgements options class.
    % It encapsulates all the information and logic around conditioning
    % objects, but cannot be instantiated outside of the 
    % judgements conditioning instructions class. It inherits from the
    % inversion options class.
    %
    % PUBLIC PROPERTIES:
    %   -> none
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> none
    %
    % DETAILS:
    %   -> This m-file defines a conditioning judgements object. It 
    %      inherits from the Inversion.Options class - see that class for
    %      details.
    %   -> The raison-d'etre of this class is to override default values
    %      defined in the Inversion.Options class to be consistent with 
    %      agreed business rules around the default options for LSS model
    %      forecast run execution in MAPS and EASE. 
    %   -> Specifically, the minimum variance over-identification option is
    %      set to false on construction, consistent with the EASE business
    %      rule that over-identified inversions should minimise the changes
    %      in the shocks, rather than their absolute values.
    %
    % NOTES:
    %   -> None
    %
    % This version: 25/11/2013
    % Author(s): Matt Waldron    

    %% PRIVATE METHODS TO JUDGEMENTS.CONDITIONING.INSTRUCTIONS OBJECTS
    methods (Access={?Judgements.Conditioning.Instructions})
        function jConditioniningOptionsObj = Options()
            % This is the constructor private constructor method.
            %
            % INPUTS:
            %   -> none
            %
            % OUTPUTS:
            %   -> jConditioniningOptionsObj: instance of the object
            %
            %% INSANTIATE INVERSION OPTIONS
            jConditioniningOptionsObj@Inversion.Options();
            %% OVER-RIDE DEFAULT OPTION FOR OVER-IDENTIFICATION
            jConditioniningOptionsObj...
                .overIdentificationIsMinimumVariance = false;
        end
    end
end