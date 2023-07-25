function FixResponsesObj = ...
    unpack_excel_fix_info_structure_and_load_instruction(...
    Model,H,FixExcelInfo)
% This helper unpacks an Excel fix info structure and loads it in to MAPS.
% 
% INPUTS:
%   -> Model: LSS model
%   -> H: forecast horizon
%   -> FixExcelInfo: structre contianing:
%       - fileName: name of the Excel file
%       - unanticipatedFixSheetName (optional): sheet containing
%         unanticipated fix instruction
%       - anticipatedFixSheetName (optional): sheet containing
%         anticipated fix instruction
%
% OUTPUTS:
%   -> FixResponsesObj: Fix responses instructions object containing:
%       - FixBase: Base forecast run dataset on which to compute the fixes
%       - Targets: Conditioning targets object defined in marginal space
%       - Instruments: Conditioing instruments object
%       - Options: Options for the inversion
%
% DETAILS:
%   -> This model analysis helper unpacks an Excel fix info structure and
%      then loads the result into MAPS.
%
% NOTES:
%   -> See the MAPS user guide for more information on model analysis 
%      functionality in MAPS.
%
% This version: 20/01/2014
% Author(s): Matt Waldron

%% CHECK INPUTS
% Note that the rest of the input checking is left to the function called
% below.
if nargin < 3
    errId = ['MAPS:',mfilename,':BadNargin'];
    generate_and_throw_MAPS_exception(errId,{num2str(nargin)}); 
elseif ~isstruct(FixExcelInfo)
    errId = ['MAPS:',mfilename,':BadInput3'];
    generate_and_throw_MAPS_exception(errId); 
end

%% UNPACK COMPONENTS OF STRUCTURE
if ~isfield(FixExcelInfo,'fileName')
    errId = ['MAPS:',mfilename,':MissingFileNameField'];
    generate_and_throw_MAPS_exception(errId); 
else
    excelFileName = FixExcelInfo.fileName;
end
if isfield(FixExcelInfo,'unanticipatedFixesSheetName')
    unanticipatedFixesSheetName = FixExcelInfo.unanticipatedFixesSheetName;
else
    unanticipatedFixesSheetName = '';
end
if isfield(FixExcelInfo,'anticipatedFixesSheetName')
    anticipatedFixesSheetName = FixExcelInfo.anticipatedFixesSheetName;
else
    anticipatedFixesSheetName = '';
end
if isempty(unanticipatedFixesSheetName) && ...
        isempty(anticipatedFixesSheetName)
    errId = ['MAPS:',mfilename,':MissingFixSheetName'];
    generate_and_throw_MAPS_exception(errId);
end

%% LOAD FIXES FROM EXCEL
FixResponsesObj = load_fix_responses_instruction_from_excel(Model,H,...
    excelFileName,unanticipatedFixesSheetName,anticipatedFixesSheetName);

end