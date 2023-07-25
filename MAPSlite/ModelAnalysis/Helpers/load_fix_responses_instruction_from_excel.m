function FixResponsesObj = load_fix_responses_instruction_from_excel(...
    Model,H,excelFileName,unanticipatedFixesSheetName,...
    anticipatedFixesSheetName)
% This helper loads a fix instruction for the fix responses from Excel.
%
% INPUTS:
%   -> Model: LSS model structure
%   -> H: forecast horizon
%   -> excelFileName: name of the Excel file
%   -> unanticipatedFixesSheetName: unanticipated fixes sheet (can be '')
%   -> anticipatedFixesSheetName: anticipated fixes sheet  (can be '')
%
% OUTPUTS:
%   -> FixResponsesObj: Fix responses instructions object containing:
%       - FixBase: Base forecast run dataset on which to compute the fixes
%       - Targets: Conditioning targets object defined in marginal space
%       - Instruments: Conditioing instruments object
%       - Options: Options for the inversion
%
% DETAILS:
%   -> This helper loads in a fix response instruction from Excel.
%   -> It checks that the information is valid and then creates a fix
%      responses instructions object.
%   -> The unanticipated and anticipated fixes must be kept in two 
%      different sheets within the same Excel file so that MAPS can
%      distinguish between them. Each sheet must be formatted such that
%      simulation dates 1:H appear in the first column, headers apppear in
%      the first row (and these must correspond to mnemonics for variables
%      in the model), and the fixes to apply and indicators for the shocks
%      to use then appear as time series underneath.
%   -> Validation of the validity of the instruction is contained within
%      the object, which will not allow invalid data to be input and which
%      contains the logic for validation of all the data in the object.
%
% NOTES:
%   -> This function is a useful helper for MAPS fix response MDE.
%
% This version: 20/01/2014
% Author(s): Matt Waldron

%% CHECK INPUTS
if nargin < 5
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
elseif ~isstruct(Model)
    errId = ['MAPS:',mfilename,':BadInput1'];
    generate_and_throw_MAPS_exception(errId);
elseif ~is_positive_real_integer(H)
    errId = ['MAPS:',mfilename,':BadInput2'];
    generate_and_throw_MAPS_exception(errId);    
elseif ~ischar(excelFileName)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId);
elseif ~ischar(unanticipatedFixesSheetName)
    errId = ['MAPS:',mfilename,':BadInput4'];
    generate_and_throw_MAPS_exception(errId);
elseif ~ischar(anticipatedFixesSheetName)
    errId = ['MAPS:',mfilename,':BadInput5'];
    generate_and_throw_MAPS_exception(errId);
elseif isempty(unanticipatedFixesSheetName) && ...
        isempty(anticipatedFixesSheetName)
    errId = ['MAPS:',mfilename,':NoFixSheetProvided'];
    generate_and_throw_MAPS_exception(errId);    
end

%% PACK STRUCTURE
% Initialise the object and then call sub-function to load, check and pack
% unanticipated and anticipated fix response instructions.
FixResponsesObj = FixResponses.Instructions(Model,H);
FixResponsesObj = load_and_pack_fixes_instruction(FixResponsesObj,Model,...
    H,excelFileName,unanticipatedFixesSheetName,'Unanticipated');
FixResponsesObj = load_and_pack_fixes_instruction(FixResponsesObj,Model,...
    H,excelFileName,anticipatedFixesSheetName,'Anticipated');

%% VALIDATE INSTRUCTIONS
try
    FixResponsesObj.validate(Model,FixResponsesObj.FixBase);
catch ValidationE
   errId = ['MAPS:',mfilename,':InvalidJudgements'];
   generate_MAPS_exception_add_cause_and_throw(ValidationE,errId);
end

end

%% HELPER FUNCTION TO LOAD, CHECK & PACK FIXES INSTRUCTION
function FixResponsesObj = load_and_pack_fixes_instruction(...
    FixResponsesObj,Model,H,excelFileName,fixesSheetName,fixType)
% This helper loads an instruction for a particular fix type from Excel.
%
% INPUTS:
%   -> FixResponsesObj: instructions object
%   -> Model: LSS model structure
%   -> H: forecast horizon
%   -> excelFileName: name of the Excel file
%   -> fixesSheetName: fixes sheet (can be '')
%   -> fixType: either 'Anticipated' or 'Unanticipated'
%
% OUTPUTS:
%   -> Fixes: updated instruction structure

%% IF FIXES SHEET NAME IS EMPTY THEN EXIT THE FUNCTION
if isempty(fixesSheetName)
    return
end

%% LOAD INSTRUCTIONS
% All time series load options are set to their defaults except: the real
% dates option which is set to "false", meaning that simulation 1...H date
% formats are expected in column 1 of each sheet; the data need not be
% complete (there could be NaN holes for a particular fix) and need not
% have a stright edge (some fixes may extend further than others).
ExcelLoadOptions.isRealDates = false;
ExcelLoadOptions.isComplete = false;
ExcelLoadOptions.isStraightEdge = false;
try
    [fixData,fixMnems] = load_time_series_data_from_excel(...
        excelFileName,fixesSheetName,ExcelLoadOptions);
catch ExcelLoadE
    errId = ['MAPS:',mfilename,':FixLoadFailure'];
    generate_MAPS_exception_add_cause_and_throw(...
        ExcelLoadE,errId,{fixType excelFileName fixesSheetName});
end

%% CHECK THAT DIMENSION OF DATA DOES NOT EXCEED FORECAST HORIZON
if size(fixData,2) > H
    errId = ['MAPS:',mfilename,':FixDataIncompatibleWithHorizon'];
    generate_and_throw_MAPS_exception(...
        errId,{fixType excelFileName fixesSheetName});
end

%% DETERMINE IF DATA LOADED IS TARGET OR INSTRUMENT
zMnems = unpack_model(Model,'zMnems');
isInstrumentData = ismember(fixMnems,zMnems);

%% ADD DATA TO OBJECT
nData = size(fixData,1);
try
    for iData = 1:nData
        if isInstrumentData(iData)
            FixResponsesObj.Instruments.(fixType).add(...
                fixMnems{iData},fixData(iData,:));
        else
            FixResponsesObj.Targets.add(fixMnems{iData},fixData(iData,:));
        end
    end
catch FixDataE
    errId = ['MAPS:',mfilename,':InvalidData'];
    generate_MAPS_exception_add_cause_and_throw(FixDataE,errId,{fixType});
end

end