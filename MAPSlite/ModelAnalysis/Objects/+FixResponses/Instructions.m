classdef Instructions < Judgements.Conditioning.Instructions
    % This m-file defines a fix responses instructions object.
    % It inherits from a Judgements.Conditioning.Instructions object with 
    % all targets information defined in marginal space. This header only 
    % discusses class members that are not defined in the super-class or 
    % that are overridden.
    %
    % PUBLIC PROPERTIES:
    %   -> FixBase: Base forecast run dataset on which to apply the fixes
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> constructor: requires a model & forecast horizon input and 
    %      returns a Judgements.Conditioning.Instructions object with
    %      targets defined in "marginal" space
    %
    % DETAILS:
    %   -> This m-file defines a fixes instructions object, which forms the
    %      basis of MAPS' LSS model fix response functionality.
    %   -> It derives from a Judgements.Conditioning.Instructions class and
    %      contains all properties and methods defined by that class.
    %   -> Note that fix responses are specified and should be interpreted
    %      as marginal changes from an arbitrary base with the precise
    %      interpretation varying by variable type. Fixes to model 
    %      variables are interpretable as levels differences, but they are
    %      also interpretable as levels given that the base for the fix is
    %      a steady state in which the model variables are all equal 0. 
    %      Model observable fixes are interpetable as levels differences, 
    %      stripping out the constant in the measurement equations. And raw 
    %      observables are interpreted as levels differences or percentage
    %      changes depending on the type of raw observable in question - 
    %      the fixes should be specified as levels differences for 
    %      stationary raw observables and percentage changes for non-
    %      stationary raw observables.
    %
    % NOTES:
    %   -> None in addition to those noted in the super class.
    %
    % This version: 14/01/2014
    % Author(s): Matt Waldron  
    
    %% PROPERTIES
    properties (SetAccess=private)
        FixBase
    end
    %% PUBLIC METHODS
    methods
        %% CONSTRUCTOR
        function FixResponsesObj = Instructions(Model,H)
            % This is the constructor method for the Conditioning class.
            %
            % INPUTS:
            %   -> Model: MAPS LSS model structure
            %   -> H: forecast horizon
            %
            % OUTPUTS:
            %   -> FixResponsesObj: instance of the fix responses object
            %   
            %% CHECK INPUTS
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),':BadConstructNargin'];
                generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
            elseif ~isstruct(Model)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput1'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~is_positive_real_integer(H)
                errId = ['MAPS:',mfilename('class'),':BadConstructInput2'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% CREATE SIMULATION JOURNEY BASE TO APPLY FIX RESPONSES
            BaseRunData = create_simulation_journey_base(Model,H);
            %% INSTANTIATE JUDGEMENTS.CONDITIONING.INSTRUCTIONS OBJECT
            % Note that this is called with the defineAsMarginals flag set
            % to true.
            FixResponsesObj@Judgements.Conditioning.Instructions(...
                Model,BaseRunData,true);
            %% SET THE FIX BASE PROPERTY
            FixResponsesObj.FixBase = BaseRunData;
        end
    end
end