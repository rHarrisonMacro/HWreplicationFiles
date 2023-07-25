function modificationConfig = get_modification_config()
% Config containing info about how to make modifications to model parts
% This config is called by the model modifier and contains information
% about all the valid modifications, and whether to get the inputs to lower
% level functions from the modification info file, or the config itself. 
% INPUTS:
%   -> None
%
% OUTPUTS:
%   -> modificationConfig: a cell array containing modification info
%
% DETAILS:
%   -> The config has as many rows as model part and modification
%   combinations and eight columns. These contain the key for the model
%   part being modified; the modification type; the name of the function
%   needed to make the modification; a vector indicating whether the inputs
%   to the function come from the parsed modification info file, or this
%   config; four inputs containing either the field name (if the vector 
%   indcated to get it from the file) or the MAPS model part name if the
%   instructions said to get it from the config.
% 
% NOTES:
%   -> Remove appears in the config before add since the function which calls
%   the config works through the different lines in order. If Add came 
%   before remove, there would be potential for confusion when a variable/
%   equation was modified by removing then adding.
%
% This version: 21/11/2012
% Author(s): Kate Reinold
modificationConfig =    {
    'MVmodType'     'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'MVinput1'}                        {'xMnems'}                                  {'xNames' 'xMnems'}                         {}
    'MVmodType'     'Add'               'add_elements_to_model_component'       [1 0]       {'MVinput1' 'MVinput2'}             {'xNames' 'xMnems'}                         {}                                          {}
    'MVmodType'     'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'MVinput1'}                        {'xMnems'}                                  {'MVinput2'}                                {'xNames'}
    'MVmodType'     'ChangeMnem'        'change_mnemonic_in_model'              [1 1 0]     {'MVinput1'}                        {'MVinput2'}                                {'xMnems'}                                  {}
    'SHmodType'     'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'SHinput1'}                        {'zMnems'}                                  {'zNames' 'zMnems'}                         {}
    'SHmodType'     'Add'               'add_elements_to_model_component'       [1 0]       {'SHinput1' 'SHinput2'}             {'zNames' 'zMnems'}                         {}                                          {}
    'SHmodType'     'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'SHinput1'}                        {'zMnems'}                                  {'SHinput2'}                                {'zNames'}
    'SHmodType'     'ChangeMnem'        'change_mnemonic_in_model'              [1 1 0]     {'SHinput1'}                        {'SHinput2'}                                {'zMnems'}                                  {}
    'MOmodType'     'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'MOinput1'}                        {'Ymnems'}                                  {'Ynames' 'Ymnems' 'YtildeTransformations'} {}
    'MOmodType'     'Add'               'add_elements_to_model_component'       [1 0]       {'MOinput1' 'MOinput2' 'MOinput3'}  {'Ynames' 'Ymnems' 'YtildeTransformations'} {}                                          {}
    'MOmodType'     'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'MOinput1'}                        {'Ymnems'}                                  {'MOinput2'}                                {'Ynames'}
    'MOmodType'     'ChangeMnem'        'change_mnemonic_in_model'              [1 1 0]     {'MOinput1'}                        {'MOinput2'}                                {'Ymnems'}                                  {}
    'MOmodType'     'ChangeEquation'    'change_element_of_model_component'     [1 0 1 0]   {'MOinput1'}                        {'Ymnems'}                                  {'MOinput2'}                                {'YtildeTransformations'}
    'MeErmodType'   'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'MeErinput1'}                      {'wMnems'}                                  {'wNames' 'wMnems'}                         {}
    'MeErmodType'   'Add'               'add_elements_to_model_component'       [1 0]       {'MeErinput1' 'MeErinput2'}         {'wNames' 'wMnems'}                         {}                                          {}
    'MeErmodType'   'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'MeErinput1'}                      {'wMnems'}                                  {'MeErinput2'}                              {'wNames'}
    'MeErmodType'   'ChangeMnem'        'change_mnemonic_in_model'              [1 1 0]     {'MeErinput1'}                      {'MeErinput2'}                              {'wMnems'}                                  {}
    'ROmodType'     'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'ROinput1'}                        {'YtildeMnems'}                             {'YtildeNames' 'YtildeMnems'}               {}
    'ROmodType'     'Add'               'add_elements_to_model_component'       [1 0]       {'ROinput1' 'ROinput2'}             {'YtildeNames' 'YtildeMnems'}               {}                                          {}
    'ROmodType'     'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'ROinput1'}                        {'YtildeMnems'}                             {'ROinput2'}                                {'YtildeNames'}
    'ROmodType'     'ChangeMnem'        'change_mnemonic_in_model'              [1 1 0]     {'ROinput1'}                        {'ROinput2'}                                {'YtildeMnems'}                             {}
    'TTmodType'     'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'TTinput1'}                        {'etatMnems'}                               {'etatNames' 'etatMnems'}                   {}
    'TTmodType'     'Add'               'add_elements_to_model_component'       [1 0]       {'TTinput1' 'TTinput2'}             {'etatNames' 'etatMnems'}                   {}                                          {}
    'TTmodType'     'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'TTinput1'}                        {'etatMnems'}                               {'TTinput2'}                                {'etatNames'}
    'TTmodType'     'ChangeMnem'        'change_mnemonic_in_model'              [1 1 0]     {'TTinput1'}                        {'TTinput2'}                                {'etatMnems'}                               {}
    'PmodType'      'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'Pinput1'}                         {'thetaMnems'}                              {'thetaNames' 'thetaMnems' 'theta'}         {}
    'PmodType'      'Add'               'add_elements_to_model_component'       [1 0]       {'Pinput1' 'Pinput2' 'Pinput3'}     {'thetaNames' 'thetaMnems' 'theta'}         {}                                          {}
    'PmodType'      'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'Pinput1'}                         {'thetaMnems'}                              {'Pinput2'}                                 {'thetaNames'}
    'PmodType'      'ChangeMnem'        'change_mnemonic_in_model'              [1 1 0]     {'Pinput1'}                         {'Pinput2'}                                 {'thetaMnems'}                              {}
    'PmodType'      'ChangeParamValue'  'change_element_of_model_component'     [1 0 1 0]   {'Pinput1'}                         {'thetaMnems'}                              {'Pinput2'}                                 {'theta'}
    'ModEqmodType'  'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'ModEqinput1'}                     {'xEqNames'}                                {'xEqNames' 'xEqStrs'}                      {}
    'ModEqmodType'  'Add'               'add_elements_to_model_component'       [1 0]       {'ModEqinput1' 'ModEqinput2'}       {'xEqNames' 'xEqStrs'}                      {}                                          {}
    'ModEqmodType'  'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'ModEqinput1'}                     {'xEqNames'}                                {'ModEqinput2'}                             {'xEqNames'}
    'ModEqmodType'  'ChangeEquation'    'change_element_of_model_component'     [1 0 1 0]   {'ModEqinput1'}                     {'xEqNames'}                                {'ModEqinput2'}                             {'xEqStrs'}
    'MEqmodType'    'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'MEqinput1'}                       {'YeqNames'}                                {'YeqNames' 'YeqStrs'}                      {}
    'MEqmodType'    'Add'               'add_elements_to_model_component'       [1 0]       {'MEqinput1' 'MEqinput2'}           {'YeqNames' 'YeqStrs'}                      {}                                          {}
    'MEqmodType'    'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'MEqinput1'}                       {'YeqNames'}                                {'MEqinput2'}                               {'YeqNames'}
    'MEqmodType'    'ChangeEquation'    'change_element_of_model_component'     [1 0 1 0]   {'MEqinput1'}                       {'YeqNames'}                                {'MEqinput2'}                               {'YeqStrs'}
    'SSmodType'     'Remove'            'remove_elements_from_model_component'  [1 0 0]     {'SSinput1'}                        {'ssMnems'}                                 {'ssNames' 'ssMnems' 'ssDefs'}              {}
    'SSmodType'     'Add'               'add_elements_to_model_component'       [1 0]       {'SSinput1' 'SSinput2' 'SSinput3'}  {'ssNames' 'ssMnems' 'ssDefs'}              {}                                          {}
    'SSmodType'     'ChangeName'        'change_element_of_model_component'     [1 0 1 0]   {'SSinput1'}                        {'ssMnems'}                                 {'SSinput2'}                                {'ssNames'}
    'SSmodType'     'ChangeMnem'        'change_mnemonic_in_model'              [1 1 0]     {'SSinput1'}                        {'SSinput2'}                                {'ssMnems'}                                 {}
    'SSmodType'     'ChangeEquation'    'change_element_of_model_component'     [1 0 1 0]   {'SSinput1'}                        {'ssMnems'}                                 {'SSinput2'}                                {'ssDefs'}
    };
end