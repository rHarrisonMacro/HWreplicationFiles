function modificationInfoFileConfig = get_modification_info_file_config
% Config containing details of modification info file formats
% A config used in conunction with the parse text file function to load
% data from a modification info file (used by the model modifier).
%
% INPUTS:
%   -> None
%
% OUTPUTS
%   -> modificationInfoFileConfig: cell array containing info about the
%   format of a modification info file
%
% DETAILS:
%   -> The config contains as many rows as the parts of the parts of a
%   model plus the metadata and has four columns. These hold: the header
%   from the modification info file indicating the model part; a flag for 
%   whether this section of the modification info file is compulsory; names
%   for each of the inputs which could be given; and a flag for whether
%   these inputs are optional or compulsory.
%
% NOTES:
%
% This version: 21/11/2012
% Author(s): Kate Reinold
modificationInfoFileConfig = {
    'METADATA'                                  'compulsory'    'metadataFields:metadataDescriptors'                'compulsory:compulsory'
    'MODEL VARIABLES'                           'optional'      'MVmodType:MVinput1:MVinput2'                       'compulsory:compulsory:optional'
    'SHOCKS'                                    'optional'      'SHmodType:SHinput1:SHinput2'                       'compulsory:compulsory:optional'
    'MODEL OBSERVABLES'                         'optional'      'MOmodType:MOinput1:MOinput2:MOinput3'              'compulsory:compulsory:optional:optional'
    'RAW OBSERVABLES'                           'optional'      'ROmodType:ROinput1:ROinput2'                       'compulsory:compulsory:optional'
    'MEASUREMENT ERRORS'                        'optional'      'MeErmodType:MeErinput1:MeErinput2'                 'compulsory:compulsory:optional'
    'TIME VARYING TRENDS'                       'optional'      'TTmodType:TTinput1:TTinput2'                       'compulsory:compulsory:optional'
    'PARAMETERS'                                'optional'      'PmodType:Pinput1:Pinput2:Pinput3'                  'compulsory:compulsory:optional:optional'
    'MODEL EQUATIONS'                           'optional'      'ModEqmodType:ModEqinput1:ModEqinput2'              'compulsory:compulsory:optional'
    'MEASUREMENT EQUATIONS'                     'optional'      'MEqmodType:MEqinput1:MEqinput2'                    'compulsory:compulsory:optional'
    'STEADY STATES & PARAMETER TRANSFORMATIONS' 'optional'      'SSmodType:SSinput1:SSinput2:SSinput3'              'compulsory:compulsory:optional:optional'
    };
end