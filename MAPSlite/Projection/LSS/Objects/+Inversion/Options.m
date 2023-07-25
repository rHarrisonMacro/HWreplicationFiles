classdef Options < handle
    % This m-file defines n inversion options class.
    % It encapsulates information about the default values of all inversion
    % options and the logic for what it is valid for users to set them to.
    %
    % PUBLIC PROPERTIES:
    %   -> overIdentificationIsMinimumVariance: true/false
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> none
    %
    % DETAILS:
    %   -> This m-file defines all the inversion options and their default
    %      values. It also provides set methods to check that user inputs 
    %      are valid.
    %
    % NOTES:
    %   -> None. 
    %
    % This version: 25/11/2013
    % Author(s): Matt Waldron
    
    %% PROPERTIES
    properties
        overIdentificationIsMinimumVariance = true;
    end
    %% PUBLIC METHODS
    methods
        %% SET METHOD FOR OVER-IDENTIFICATION IS MINIMUM VARIANCE
        function set.overIdentificationIsMinimumVariance(...
                iOptionsObj,isMinVar)
            if ~is_logical_scalar(isMinVar)
                errId = ['MAPS:',...
                    mfilename('class'),':BadOverIdentificationInput'];
                generate_and_throw_MAPS_exception(errId);
            end
            iOptionsObj.overIdentificationIsMinimumVariance = isMinVar;
        end
    end
end