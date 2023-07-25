classdef TimeVaryingTrends < Judgements.Data.Values
    % This m-file defines an LSS model time trends judgements object.
    % This object forms part of a MAPS LSS model judgement instructions 
    % object as encapsulated by the classes that form part of the 
    % Judgements package and connot be instantiated outside of the 
    % Judgements.Instructions class. It inherits from the
    % Judgements.Data.Values class. This header only discusses additional
    % members that are defined in this class.
    %
    % PUBLIC PROPERTIES:
    %   -> none
    %
    % EVENTS:
    %   -> none
    %
    % PUBLIC METHODS:
    %   -> applyJudgementToForecastRun: updates content of forecast run 
    %      with the judgements
    %
    % DETAILS:
    %   -> This m-file defines a time-varying trends judgements 
    %      instructions object, which can be used to update a MAPS forecast
    %      run dataset for new judgemental time-trend values as part of LSS
    %      model impose judgement functionality.
    %   -> This class can only be instantiated as part of a
    %      Judgements.Instructions object.
    %   -> This object exposes all the public methods and properties that
    %      form part of the Judgements.Data.Values class. These include
    %      "add", "overwrite" and "remove" methods, providing flexibility 
    %      over how the judgement data is set.
    %   -> This class also contains a method and the logic for updating the
    %      time-varying trends in the forecast run dataset structure as 
    %      part of the imposition of judgement - see the MAPS LSS model 
    %      forecast run macros.
    %   -> Notice that an exception is thrown on editing if the model being
    %      used does not have time-varying trends or the forecast run
    %      structure does not contain them (i.e. is inconsistent with the
    %      model).
    %
    % NOTES:
    %   -> There is nothing to stop there being an inconsistency between
    %      the construction of the object and the forecast run data 
    %      structure being updated. There is no way of validating for that
    %      or of eliminating the problem without physically storing the run
    %      data structure on the object, which would take up memory and
    %      could reduce flexibility.
    %
    % This version: 09/12/2013
    % Author(s): Matt Waldron
    
    %% PUBLIC METHODS
    methods
        %% UPDATE FORECAST RUN
        function RunData = applyJudgementToForecastRun(jTTobj,RunData)
            % This method updates a forecast run dataset for judgements.
            % Specifically, it updates the time-varying trends forecast 
            % data for judgements made.
            %
            % INPUTS:
            %   -> jTTobj: time-varying trends judgements object
            %   -> RunData: LSS model forecast run structure
            %
            % OUTPUTS:
            %   -> RunData: updated forecast run structure
            %
            %% CHECK INPUT
            if nargin < 2
                errId = ['MAPS:',mfilename('class'),...
                    ':BadApplyJudgementToForecastDatasetNargin'];
                generate_and_throw_MAPS_exception(errId);
            elseif ~isstruct(RunData)
                errId = ['MAPS:',mfilename('class'),...
                    ':BadApplyJudgementToForecastDatasetInput1'];
                generate_and_throw_MAPS_exception(errId);
            end
            %% UPDATE TIME TRENDS FORECAST DATA MATRIX (IF APPLICABLE)
            if jTTobj.isNonEmpty
                RunData.Forecast.timeVaryingTrends = ...
                    jTTobj.applyJudgementToForecastDataset(...
                    RunData.Forecast.timeVaryingTrends);
            end
        end
    end
    %% METHODS ACCESSIBLE BY JUDGEMENTS.INSTRUCTIONS CLASS
    methods (Access={?Judgements.Instructions})
        %% CONSTRUCTOR
        function jTTobj = TimeVaryingTrends(Model,H)
            % Constructor method for this class.
            %
            % INPUTS:
            %   -> Model: LSS model structure
            %   -> H: forecast horizon scalar
            %
            % OUTPUTS:
            %   -> jTTobj: instance of the object
            %
            %% CHECK IF MODEL HAS TIME-VARYING TRENDS
            modelHasTimeVaryingTrends = unpack_model(...
                Model,'modelHasTimeVaryingTrends');
            %% UNPACK MNEMONICS IF APPLICABLE
            % If the model does not have time-varying trends, then the
            % mnemonics are set to empty (and it won't be possible to edit
            % the object).
            if modelHasTimeVaryingTrends
                ttMnems = unpack_model(Model,'etatMnems');
            else
                ttMnems = {''};                
            end
            %% INITIALISE TIME TRENDS JUDGEMENTS OBJECT
            jTTobj@Judgements.Data.Values(...
                ttMnems,H,~modelHasTimeVaryingTrends);
        end
    end
end