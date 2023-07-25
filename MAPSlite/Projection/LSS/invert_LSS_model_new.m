function NewShocks = invert_LSS_model_new(InversionObj,B,PHI,F,xT)
% This function implements the inversion algorithm in Appendix C of the WP.
% It inverts the solution of a linear state space (LSS) model to find the
% the values for the anticipated, and/or unanticipated shocks, that satisfy
% a set of conditioning paths for model variables. These shocks can then be
% used to produce a new projection consistent with the judgement(s) (i.e.
% to condition the projection on those judgements).
%
% INPUTS:
%   -> InversionObj: inversion instructions object containing:
%       - Targets: targets inversion object containing:
%           - values: values inversion data object
%           - indices: indices inversion data object
%           - weights (optional/inversion dep.t): weights inversion data
%             object
%       - Instruments: instruments inversion object containing:
%           - Anticipated (optional/model dep.): 
%               - indices: indices inversion data object
%               - weights (optional/inversion dep.): weights inversion data
%                 object
%           - Unanticipated (optional): 
%               - indices: indices inversion data object
%               - weights (optional/inversion dep.): weights inversion data
%                 object
%       - Options: inversion options object containing:
%           - overIdentificationIsMinimumVariance: true or false
%   -> B: nx*nx loadings matrix on the lagged endogenous variables
%   -> PHI: nx*nz loadings matrix on the shocks
%   -> F: nx*nx matrix of loadings on one-period ahead anticipated shocks
%      (zeros in backward looking models)
%   -> xT: nx*1 vector of initial conditions for the endogenous variables
%
% OUTPUTS:
%   -> NewShocks: structure containing a new set of shocks
%       - anticipated (model dependent): nz*H matrix of shocks
%       - unanticipated: nz*H matrix of shocks
%
% DETAILS:
%   -> This function inverts an LSS model to find the set of shocks that 
%      deliver a set of conditioning paths using a combination of 
%      anticipated and unanticipated shocks.
%   -> It implements both over-identified and under-identified inversions 
%      (as well as exactly-identified).
%   -> In the case of over-identified inversions, it contains two 
%      algorithms that can be used to "fix" the endogenous variables: a 
%      minimum change algorithm which seeks to minimise the (weighted) sum
%      squared deviation of the shocks used to implement the inversion from
%      their existing values (overIdentificationIsMinimumVariance = false);
%      a minimum variance algorithm which seeks to minimise the (weighted) 
%      sum squared values of the shocks used to implement the inversion 
%      regardless of their previous values
%      (overIdentificationIsMinimumVariance = true).
%   -> Regardless of the particular case, the algorithm outlined in 
%      Appendix C of the forecasting platform working paper works by 
%      reordering the model solution, partitioning it into vectors that are
%      known and then inverting to find the unknowns (which are the values
%      for the shocks that deliver the inversion).
%   -> The algorithm also permits an optional set of weights to be applied
%      to the minimisation problems. In the case of over-identified
%      inversions, those weights apply to the shocks being used. In the
%      case of under-identified inversions those weights apply to the
%      target (conditioning) values. This means that this (optional) input
%      must be set in a way that is consistent with the type of inversion
%      being implemented (and will fail validation if not). In addition, if
%      weighting is being applied then weights for all the variables and 
%      shocks must be supplied in all the time periods (i.e. it is an all 
%      or nothing option).
%   -> Note that another way of calling this function is through the
%      "invert" method on the InversionObj class. This method validates
%      that the inputs to this function are consistent with each other.
%      That validation is by-passed in this function (which means it can be
%      used for off-line experiments where performance is important, but
%      validation less so, whereas inversion that support "project"
%      operations in EASE are always routed through the invert method).
%      This means that it should only be called directly at users' own
%      risks - passing the "wrong" inputs into this function could lead to
%      unexpected behaviour and errors.
%
% NOTES:
%   -> The notation used here is an attempt to be consistent with the
%      notation used in Appendix C of the working paper. For example, the
%      TeX notation of $\mathbb{BB^TT}$ is translated here as BBTT.
%   -> Note that the algorithm and the object are coded for the general 
%      case, but it implicitly allows for specific cases. For example, if 
%      the inversion only includes anticipated shocks, then the 
%      unanticipated shock usage indices are set to empty (zeros(0,1)) in 
%      every period. The matrix algebra follows through correctly without
%      having to litter the code with a bunch of "if"-"else" or "switch"-
%      "case" statements.
%   -> Some effort has been put into the performance of this function. In
%      particular, the most expensive step is the creation of the 
%      structural matrices in "build_partitioned_stacked_matrices". That
%      sub-function contains some commentary and notes which explain the
%      steps that were taken to improve performance and some suggestions
%      about how performance might be improved even further in the future.
%
% This version: 20/11/2013
% Author(s): Matt Waldron

%% CHECK NUMBER OF INPUTS
if nargin < 5
   errId = ['MAPS:',mfilename,':BadNargin'];
   generate_and_throw_MAPS_exception(errId,{num2str(nargin)});
end

%% UNPACK CONTENT OF INVERSION INSTRUCTIONS OBJECT
% For example, the conditioning target values ({xbar^{i}_{T+h}}_{h=0}^{H} 
% in the documentation) are stored as column vectors in a 1*H cell array 
% with a corresponding set of indices for the position of the variables in
% the model in an identically sized cell array. Note that all of these sets
% are complete in the sense that they span the whole forecast horizon. For
% example, if there are no anticipated shocks being used as instruments in
% any time period, then aibarSet, aiWeightSet and aiIndSet are 1*H cell 
% arrays of zeros(0,1) vectors with naiSet = zeros(1,H) and nai = 0. This
% approach allows the algebra below to proceed as in the general case.
[nx,nz,H,inversionType,inversionIncludesWeights,xibarSet,...
    xiWeightSet,xiIndSet,nxiSet,nxi,aibarSet,aiWeightSet,aiIndSet,...
    naiSet,nai,anbarSet,anIndSet,nanSet,nan,uibarSet,uiWeightSet,...
    uiIndSet,nuiSet,nui,unbarSet,unIndSet,nunSet,nun,...
    overIdentificationIsMinimumVariance] = ...
    unpack_inversion_instructions_object(InversionObj);

%% COMPUTE THE OPERATOR MATRICES REQUIRED TO REORDER THE SYSTEM
% Call a sub-function to compute the matrices required to reorder the
% model solution appopriately in each of the periods of the inversion. For
% example, TxSet is a 1*H cell array with TxSet{h} containing an nx*nx 
% matrix to reorder the model variables in period T+h such that:
% TxSet{h}*x_{T+h} = xT_{T+h} = [xi_{T+h};xn_{T+h}].
[TxSet,TaSet,TuSet,TxIsIdenticalToTxLag,TaIsIdenticalToTaLag,...
    TuIsIdenticalToTuLag] = compute_reorder_operators(...
    nx,nxiSet,xiIndSet,nz,naiSet,aiIndSet,...
    nanSet,anIndSet,nuiSet,uiIndSet,nunSet,unIndSet,H);

%% COMPUTE THE REORDERED MODEL SOLUTION MATRICES
% Call a sub-function to reorder the model solution appopriately in each of
% the periods of the inversion. For example, BTTset is a 1*H cell with 
% BTTset{h} containing B reordered appropriately in forecast period h to 
% deliver the reordered matrices from the derivation:
% BTTset{h} = [B^{ii}_{T+h|T+h-1} B^{in}_{T+h|T+h-1}
%              B^{ni}_{T+h|T+h-1} B^{nn}_{T+h|T+h-1}].
[BTTset,FkPHITTaSet,PHITTuSet] = ...
    reorder_model_solution_matrices(B,F,PHI,TxSet,TaSet,TuSet,...
    TxIsIdenticalToTxLag,TaIsIdenticalToTaLag,TuIsIdenticalToTuLag,H,nx);

%% BUILD INVERSION MATRICES
% Build the inversion matrices as detailed in the derivation
[BBTT,BTT,ATi,ATn,UTi,UTn,BiT,Aii,Ain,Uii,Uin] = ...
    build_partitioned_stacked_matrices(BTTset,FkPHITTaSet,PHITTuSet,...
    nxiSet,nxi,naiSet,nai,nanSet,nan,nuiSet,nui,nunSet,nun,nx,H);

%% STACK VALUES FOR CONDITIONING PATHS & SHOCKS NOT BEING USED
% Call a sub-function to stack conditioning paths and shocks not being used
% in the inversion across the whole horizon H in individual vectors as 
% defined in the derivation. For example,
% xibar = [xibarSet{1};xibarSet{2};...;xibarSet{H-1};xibarSet{H}];
[xibar,zibar,anbar,unbar,xiWeightMat,ziWeightMat] = ...
    stack_conditioning_paths_shocks_and_weights(...
    xibarSet,anbarSet,unbarSet,aibarSet,uibarSet,...
    xiWeightSet,aiWeightSet,uiWeightSet);

%% COMPUTE MATRICES IN SIMULTANEOUS SYSTEM
% Call a sub-function to compute the constant & loading in the siumltanous
% system of equations such that: xi = Wzi+C, where zi = [ai;ui].
[W,C] = compute_matrices_in_simultaneous_system(xT,anbar,unbar,...
    BBTT,BTT,ATi,ATn,UTi,UTn,BiT,Aii,Ain,Uii,Uin);

%% CHECK FEASIBILITY OF INVERSION
% Check that the inversion is feasible. The check is based on the rank of
% W'W in the under-identified case and WW' in the over-identified case. 
% Note that an extension here would be to explore the reasons for failure 
% to meet this rank condition more carefully to come up with some more 
% specific checks.
check_inversion_is_feasible(W,nxi,nai,nui,inversionType);

%% SETUP WEIGHTS
% This allows the inversion below to proceed regardless of whether weights
% were included or not.
omega = setup_inversion_weights(inversionType,inversionIncludesWeights,...
    xiWeightMat,ziWeightMat,nxi,nai,nui);

%% IMPLEMENT THE INVERSION
% Invert the system to compute the shocks, zi = [ai;ui], depending on
% whether the inversion is over- or under-identified and, if the former,
% the choice of identification scheme. Note that exactly identified
% inversions are treated as under-identified inversions - it would be
% equally valid to treat them as over-identified.
switch inversionType
    case 'over-identified'
        if overIdentificationIsMinimumVariance
            zi = ((omega\W')/((W/omega)*W'))*(xibar-C);
        else
            zi = zibar+((omega\W')/((W/omega)*W'))*(xibar-C-W*zibar);
        end
    otherwise
        zi = ((W'*omega*W)\(W'*omega))*(xibar-C);
end

%% CREATE A COMPLETE MATRIX OF SHOCKS ACROSS ALL TIME PERIODS
% Call a sub-function to combine the shocks and reorder them such that a &
% u are nz*H matrices with, for example, a(:,1) containing the original,
% model ordering of the shocks for period s = 1 of the inversion.
[a,u] = combine_and_reorder_shocks(...
    zi,naiSet,nuiSet,anbar,nanSet,unbar,nunSet,TaSet,TuSet,nz,H);

%% PACK SHOCKS OUTPUT
% Only the shocks that were included on construction of the inversion
% instructions object are output.
if InversionObj.Instruments.allowAnticipatedInstruments
    NewShocks.anticipated = a;
end
if InversionObj.Instruments.allowUnanticipatedInstruments
    NewShocks.unanticipated = u;
end

end

%% FUNCTION TO COMBINE & REORDER SHOCKS AT THE END OF THE INVERSION
function [a,u] = combine_and_reorder_shocks(...
    zi,naiSet,nuiSet,anbar,nanSet,unbar,nunSet,TaSet,TuSet,nz,H)
% This helper creates a complete model ordered set of shocks.
% It separates out the anticipated and unanticipated shocks computed in the
% inversion. It then combines them with the shocks that were not used to
% achieve the inversion before reordering the vector using the reordering
% operators.
%
% INPUTS:
%   -> zi: combined set of shocks computed in the inversion
%   -> naiSet: number of anticipated shocks in inversion in each period
%   -> nuiSet: number of unanticipated shocks in inversion in each period
%   -> anbar: stacked values for anticipated shocks not used
%   -> nanSet: number of anticipated shocks not used each period
%   -> unbar: stacked values for unanticipated shocks not used
%   -> nunSet: number of unanticipated shocks not used each period
%   -> TaSet: set of reorder operators for the anticipated shocks
%   -> TuSet: set of reorder operators for the unanticipated shocks
%   -> nz: number of shocks in the model
%   -> H: forecast horizon
%
% OUTPUTS:
%   -> a: complete matrix of anticipated shocks
%   -> u: complete matrix of unanticipated shocks

%% SEPARATE OUT THE INVERSION SHOCKS
% Extract the anticipated shocks and the unanticipated shocks computed in
% the inversion using the fact that inversion orders the anticipated shocks
% first.
ai = zi(1:sum(naiSet));
ui = zi(sum(naiSet)+1:sum(naiSet)+sum(nuiSet));

%% INITIALISE
aT = zeros(nz,H);
uT = zeros(nz,H);
a = zeros(nz,H);
u = zeros(nz,H);

%% COMPUTE THE SHOCKS
% For each period of the inversion from 1 to H compute the inversion
% ordered shock vector by stacking the inverted shocks on top of the
% shocks being held fixed. Use the reorder operator for that period to
% compute the model ordered shocks.
for h = 1:H
    aT(:,h) = [ai(sum(naiSet(1:h-1))+1:sum(naiSet(1:h)));
               anbar(sum(nanSet(1:h-1))+1:sum(nanSet(1:h)))];
    uT(:,h) = [ui(sum(nuiSet(1:h-1))+1:sum(nuiSet(1:h)));
               unbar(sum(nunSet(1:h-1))+1:sum(nunSet(1:h)))];
    a(:,h) = TaSet{h}'*aT(:,h);
    u(:,h) = TuSet{h}'*uT(:,h);
end

end

%% FUNCTION TO SETUP INVERSION WEIGHTS
function omega = setup_inversion_weights(inversionType,...
    inversionIncludesWeights,xiWeightMat,ziWeightMat,nxi,nai,nui)
% This helper sets-up the weights for the inversion.
% It is designed so that "neutral" weights are created in cases where no
% weights were input. This allows the inversion to proceed on the basis
% that the inversion is always weighted (thereby avoiding if cases and
% repeated code).
%
% INPUTS:
%   -> inversionType: inversion type string
%   -> inversionIncludesWeights: true/false
%   -> xiWeightMat: target weights matrix (could be empty)
%   -> ziWeightMat: instrument weights matrix (could be empty)
%   -> nxi: number of targets
%   -> nai: number of anticipated instruments
%   -> nui: number of unanticipated instruments
%
% OUTPUTS:
%   -> omega: either nxi*nxi or nzi*nzi matrix of weights (depedning on the
%      inversion type)

%% CREATE WEIGHTS MATRIX DEPENDING ON INVERSION TYPE & ANY INPUT WEIGHTS
switch inversionType
    case 'over-identified'
        if inversionIncludesWeights
            omega = ziWeightMat;
        else 
            omega = eye(nai+nui);
        end
    otherwise
        if inversionIncludesWeights
            omega = xiWeightMat;
        else
            omega = eye(nxi);
        end
end

end

%% FUNCTION TO CHECK THAT INVERSION IS FEASIBLE
function check_inversion_is_feasible(W,nxi,nai,nui,inversionType)
% This function checks inversion feasibility based on a rank condition.
% The rank condition tested depends on the type of inversion.
%
% INPUTS:
%   -> W: nxi*nzi matrix of loadings on shocks being used in the inversion
%      (anticipated stacked on unanticipated)
%   -> nxi: number of conditioning data points
%   -> nai: number of anticipated instrument data points
%   -> nui: number of unanticipated instrument data points
%   -> inversionType: inversion type string
%
% OUTPUTS:
%   -> none

%% CHECK RANK
switch inversionType
    case 'over-identified'
        if rank(full(W*W'),1e-12) < nxi
            errId = ['MAPS:',mfilename,':InfeasibleInversion'];
            errArgs = {'WW'' in this ',inversionType,' case'};
            generate_and_throw_MAPS_exception(errId,errArgs);
        end
    otherwise
        if rank(full(W'*W),1e-12) < nai+nui
            errId = ['MAPS:',mfilename,':InfeasibleInversion'];
            errArgs = {'W''W in this ',inversionType,' case'};
            generate_and_throw_MAPS_exception(errId,errArgs);            
        end
end

end

%% FUNCTION TO COMPUTE THE MATRICES IN THE SIMULTANEOUS SYSTEM
function [W,C] = compute_matrices_in_simultaneous_system(xT,anbar,unbar,...
    BBTT,BTT,ATi,ATn,UTi,UTn,BiT,Aii,Ain,Uii,Uin)
% This function computes the matrices, W & C, in the simultaneous system.
% It follows the notation in the derivation.
%
% INPUTS:
%   -> xT: nx*1 vector of initial conditions for the projection
%   -> anbar: stacked vector of values for anticipated shocks not used
%   -> unbar: stacked vector of values for unanticipated shocks not used
%   -> BBTT: (nx*H)*(nx*H) matrix of loadings for effects of past
%      shocks on lagged endogenous variables
%   -> BTT: (nx*H)*nx matrix of loadings for effect of initial 
%      conditions on lagged endogenous variables
%   -> ATi: (nx*H)*nai matrix of loadings for impact of past anticipated 
%      shocks being used in the inversion
%   -> ATn: (nx*H)*nan matrix of loadings for impact of past anticipated 
%      shocks not being used in the inversion
%   -> UTi: (nx*H)*nui matrix of loadings for impact of past unanticipated 
%      shocks being used in the inversion
%   -> UTn: (nx*H)*nun matrix of loadings for impact of past unanticipated 
%      shocks not being used in the inversion
%   -> BiT: nxi*(nx*H) matrix of loadings for contemporaneous impact of 
%      lagged endogenous variables on endogenous variables being 
%      conditioned
%   -> Aii: nxi*nai matrix of loadings for contemporaneous impact of 
%      anticipated shocks used on endogenous variables being conditioned
%   -> Ain: nxi*nan matrix of loadings for contemporaneous impact of 
%      anticipated shocks not used on endogenous variables being conditioned
%   -> Uii: nxi*nui matrix of loadings for contemporaneous impact of 
%      unanticipated shocks used on endogenous variables being conditioned
%   -> Uin: nxi*nun matrix of loadings for contemporaneous impact of 
%      unanticipated shocks not used on endogenous variables being 
%      conditioned
%
% OUTPUTS:
%   -> W: nxi*nzi matrix of loadings on shocks being used in the inversion
%      (anticipated stacked on unanticipated)
%   -> C: nxi*1 vector of constants

%% COMPUTE COMPONENTS OF SYSTEM
Wa = BiT*BBTT*ATi+Aii;
Wu = BiT*BBTT*UTi+Uii;
C = BiT*(BTT*xT+BBTT*(ATn*anbar+UTn*unbar))+Ain*anbar+Uin*unbar;

%% COMBINE THE SHOCK LOADING MATRICES
W = [Wa Wu]; 

end

%% BUILD PARTITIONED STACKED MATRICES REWORKED
function [BBTT,BTT,ATi,ATn,UTi,UTn,BiT,Aii,Ain,Uii,Uin] = ...
    build_partitioned_stacked_matrices(...
    BTTset,FkPHITTaSet,PHITTuSet,nxiSet,nxi,naiSet,nai,nanSet,nan,...
    nuiSet,nui,nunSet,nun,nx,H)
% This function computes the partitioned stacked matrices for inversion.
% It (loosely) follows the notation in the derivation.
%
% INPUTS:
%   -> BTTset: 1*H cell array of reordered B matrices
%   -> FkPHITTaSet: H*H upper diagonal cell array of reordered 
%      {F^kPhi}^{TT^a} matrices
%   -> PHITTuSet: 1*H cell array of reordered Phi^{TT^u} matrices
%   -> nxiSet: 1*H vector with number of variables being conditioned in 
%      each period
%   -> nxi: total number of variables being conditioned across all periods
%   -> naiSet: 1*H vector with number of anticipated shocks being used in
%      the inversion in each period
%   -> nai: total number of anticipated shocks used across all periods
%   -> nanSet: 1*H vector with number of anticipated shocks not being used
%      in the inversion in each period
%   -> nan: total number of anticipated shocks not used across all periods
%   -> nuiSet: 1*H vector with number of unanticipated shocks being used in
%      the inversion in each period
%   -> nui: total number of unanticipated shocks used across all periods
%   -> nunSet: 1*H vector with number of unanticipated shocks not being
%      used in the inversion in each period
%   -> nun: total number of unanticipated shocks not used across all 
%      periods
%   -> nx: number of endogenous variables
%   -> H: number of forecast periods
%
% OUTPUTS:
%   -> BBTT: (nx*H)*(nx*H) matrix of loadings for effects of past
%      shocks on lagged endogenous variables
%   -> BTT: (nx*H)*nx matrix of loadings for effect of initial 
%      conditions on lagged endogenous variables
%   -> ATi: (nx*H)*nai matrix of loadings for impact of past anticipated 
%      shocks being used in the inversion
%   -> ATn: (nx*H)*nan matrix of loadings for impact of past anticipated 
%      shocks not being used in the inversion
%   -> UTi: (nx*H)*nui matrix of loadings for impact of past unanticipated 
%      shocks being used in the inversion
%   -> UTn: (nx*H)*nun matrix of loadings for impact of past unanticipated 
%      shocks not being used in the inversion
%   -> BiT: nxi*(nx*H) matrix of loadings for contemporaneous impact of 
%      lagged endogenous variables on endogenous variables being 
%      conditioned
%   -> Aii: nxi*nai matrix of loadings for contemporaneous impact of 
%      anticipated shocks used on endogenous variables being conditioned
%   -> Ain: nxi*nan matrix of loadings for contemporaneous impact of 
%      anticipated shocks not used on endogenous variables being conditioned
%   -> Uii: nxi*nui matrix of loadings for contemporaneous impact of 
%      unanticipated shocks used on endogenous variables being conditioned
%   -> Uin: nxi*nun matrix of loadings for contemporaneous impact of 
%      unanticipated shocks not used on endogenous variables being 
%      conditioned
%
% NOTES:
%   -> This function contains the logic for creation of the structural
%      matrices necessary to perform the inversion.
%   -> The logic for building up each of the matrices is contained in 
%      a separate sub-function. The notation used for each matrix is
%      designed to be consistent with the notation used in Appendix C of
%      the forecasting platform WP (for instance \mathbb{BB^{TT}} is BBTT.
%   -> It is quite likely that there is a slightly more efficient
%      implementation because there is some repetition across the sub-
%      functions (even after taking into account that the bulk of the 
%      repeated logic is factored into a helper sub-function which each of
%      these sub-functions calls). For example, index vectors referring to
%      a block row of the reordered model variables are created in the same 
%      way in multiple places. However, the profiler reveals that there 
%      would be little performance benefit in building up the matrices in a
%      single loop, so the implementation here was preferred because it is
%      much easier to follow what is going on.
%   -> Note that a key part of this and any other viable implementation is
%      the use of sparse matrices. Moreover, these sparse matrices have to
%      be built "by hand" because large inversions will lead to out of 
%      memory issues if the matrices are first created as "full"
%      matrices (which is the easiest implementation). Given this 
%      requirement, the logic of the implementation below is to build up 
%      the sparse matrices in the most efficient way (otherwise performance
%      is severely compromised). For all but one of the matrices created 
%      below it turns out (after reading the MATLAB documnetation and
%      trying several alternative approaches) that the best way to do this
%      is to build up vectors of row, column and value triplets before 
%      calling the MATLAB sparse matrix builder function (which is 
%      expensive). However, for the BBTT matrix it was more efficient to 
%      build a sparse matrix for each block row being added (paying the 
%      additional cost of calling the "sparse" function more often) and 
%      adding them together (recrusively, starting from the ultimate spare 
%      matrix of zeros). This reflects that the cost of indexing into the 
%      vectors of indices and values being used to build up the sparse 
%      matrix becomes a lot bigger as the size of the matrix being built is
%      bigger - the BBTT matrix is (usually) easily the biggest matrix to 
%      be built and is roughly of dimension (nx^2*H0^2)/2, so can be very 
%      big in medium to large scale models. 
%   -> Each individual sub-function below contains the logic for building
%      each matrix (with, as discussed above, that logic being similar for
%      the majority of the matrices with the BBTT matrix handled in a 
%      special way). The logic for the creation of the sparse indices, 
%      values and then matrices is contained in two helper functions: 
%      "append_sparse_indices_and_values_with_new_block" and 
%      "build_sparse_matrix_from_value_and_index_vectors".
%   -> I think the best bets for further performance improvements (which I
%      think would only be worth considering if this function was being 
%      used frequently to implement long inversions in large models (i.e. 
%      big H, H0 & nx)) are: a) reduce the dimensionality of the problem 
%      further up-stream by eliminating rows and columns of the model 
%      solution matrices that are not necessary in order to compute the new
%      shocks (a bit like compute_minimum_state_space_representation with 
%      some additional logic); b) exploit more of the special cases that 
%      exist when the instructions for the inversion do not vary over time
%      (eg when BTTset{h} = BTTset{h-1} etc); c) revisit the maths/logic of
%      the problem to see if there is a more efficient way of doing it.

%% COMPUTE THE MAXIMUM PERIOD UP TO WHICH VARIABLES ARE BEING CONDITIONED
% The logic of the maths only requires the above matrices to be filled in
% up to an including the (block) row corresponding to the final period in
% which any variables are being conditioned.
H0 = find(nxiSet>0,1,'last');

%% BUILD MATRICES
BBTT = build_BBTT_matrix(BTTset,nx,H0);
BTT = build_BTT_matrix(BTTset,nx,H0);
ATi = build_ATi_matrix(FkPHITTaSet,naiSet,nx,nai,H0,H);
ATn = build_ATn_matrix(FkPHITTaSet,naiSet,nanSet,nx,nan,H0,H);
UTi = build_UTi_matrix(PHITTuSet,nuiSet,nx,nui,H0);
UTn = build_UTn_matrix(PHITTuSet,nuiSet,nunSet,nx,nun,H0);
BiT = build_BiT_matrix(BTTset,nxiSet,nxi,nx,H0);
Aii = build_Aii_matrix(FkPHITTaSet,nxiSet,naiSet,nxi,nai,H0,H);
Ain = build_Ain_matrix(FkPHITTaSet,nxiSet,naiSet,nanSet,nxi,nan,H0,H);
Uii = build_Uii_matrix(PHITTuSet,nxiSet,nuiSet,nxi,nui,H0);
Uin = build_Uin_matrix(PHITTuSet,nxiSet,nuiSet,nunSet,nxi,nun,H0);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING BBTT MATRIX
function BBTT = build_BBTT_matrix(BTTset,nx,H0)
% This helper contains the logic for building the BBTT matrix.
%
% INPUTS:
%   -> BTTset: 1*H cell array set of reorderd B matrices across time
%   -> nx: total number of endogenous variables
%   -> H0: horizon up to which variables are being conditioned
%
% OUTPUTS:
%   -> BBTT: BBTT matrix

%% INITIALISE SPARSE MATRIX
% This is what will be returned if H0==1.
BBTT = sparse(nx*H0,nx*H0);

%% ADD IDENTITY PARTS OF BBTT
% If conditioing information is being applied for more than one period,
% then the BBTT matrix will include (H0-1) block identity matrices on the
% off-diagonal.
if H0 > 1
    nSparseEntries = (H0-1)*nx;
    sparseRowInds = NaN*ones(nSparseEntries,1);
    sparseColInds = sparseRowInds;
    sparseVals = sparseRowInds;
    for h = 2:H0
        hBlockRowInds = ((h-1)*nx+1:h*nx)';
        hBlockColInds = (h-2)*nx+1:(h-1)*nx;
        hBlockEyeSparseInds = hBlockColInds';
        sparseRowInds(hBlockEyeSparseInds) = hBlockRowInds;
        sparseColInds(hBlockEyeSparseInds) = hBlockColInds';
        sparseVals(hBlockEyeSparseInds) = ones(nx,1);
    end
    BBTT = build_sparse_matrix_from_value_and_index_vectors(...
        sparseRowInds,sparseColInds,sparseVals,nx*H0,nx*H0);
end

%% ADD LAGGED B PRODUCT PARTS OF BBTT MATRIX
% If conditioing information is being applied for more than two periods,
% then the BBTT matrix will include products of the lags of the state
% transition matrix, B. A sparse matrix of the same dimension as the BBTT 
% matrix is created for each block row and these are recursively added to 
% the BBTT matrix created above. This implementation turns out to be 
% quicker than the implementation used in the sister sub-functions below -
% see the comments in the calling (parent) function for details. 
if H0 > 2
    hBlockRowMultFactors = zeros(nx,H0*nx);
    hBlockRowMultFactors(:,1:nx) = eye(nx,nx);
    for h = 3:H0
        hBlockRowInds = ((h-1)*nx+1:h*nx)';
        hBlockColInds = (1:(h-2)*nx);
        hLagBTT = BTTset{h-1};
        hBlockRowMultFactors(:,hBlockColInds) = ...
            hLagBTT*hBlockRowMultFactors(:,hBlockColInds);
        hEyeBlockColInds = (h-2)*nx+1:(h-1)*nx;
        hBlockRowMultFactors(:,hEyeBlockColInds) = eye(nx,nx);
        hBlock = hBlockRowMultFactors(:,hBlockColInds);
        blockRowIndsAcrossCols = hBlockRowInds*ones(1,(h-2)*nx);
        blockColIndsAcrossRows = ones(nx,1)*hBlockColInds;
        nonZeroValInds = find(hBlock);
        sparseRowInds = blockRowIndsAcrossCols(nonZeroValInds);
        sparseColInds = blockColIndsAcrossRows(nonZeroValInds);
        sparseVals = hBlock(nonZeroValInds);
        BBTT = BBTT+...
            sparse(sparseRowInds,sparseColInds,sparseVals,nx*H0,nx*H0);
    end
end

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING BTT MATRIX
function BTT = build_BTT_matrix(BTTset,nx,H0)
% This helper contains the logic for building the BTT matrix.
%
% INPUTS:
%   -> BTTset: 1*H cell array set of reorderd B matrices across time
%   -> nx: total number of endogenous variables
%   -> H0: horizon up to which variables are being conditioned
%
% OUTPUTS:
%   -> BTT: BTT matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
% The BTT matrix is a block column vector made up of products of the state
% transition matrix, B, and an identity matrix.
nSparseEntries = (H0-1)*nx^2+nx;

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% INITIALISE MULTIPLICATION FACTORS TO CARRY DOWN THE BLOCK ROWS
hBlockRowMultFactors = eye(nx,nx);

%% ADD IDENTITY AS FIRST BLOCK ROW
sparseRowInds(1:nx) = (1:nx)';
sparseColInds(1:nx) = (1:nx)';
sparseVals(1:nx) = ones(nx,1);

%% LOOP OVER BLOCK ROWS
% At the start of each loop the recursive B multiplication factor is
% set by multiplying the recursive factor by the lag of the B matrix. The
% sparse indices and values are then updated to include this matrix.
nEntriesInserted = nx;
hMatRowInds = (1:nx)';
hMatColInds = (1:nx);
hBlockColInds = (1:nx);
for h = 2:H0
    hBlockRowMultFactors = BTTset{h-1}*hBlockRowMultFactors;
    hBlockRowInds = ((h-1)*nx+1:h*nx)';
    [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
        append_sparse_indices_and_values_with_new_block(...
        sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
        hBlockRowInds,hBlockColInds,hBlockRowMultFactors,hMatRowInds,...
        hMatColInds);
end

%% BUILD SPARSE MATRIX
BTT = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nx*H0,nx);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING ATi MATRIX
function ATi = build_ATi_matrix(FkPHITTaSet,naiSet,nx,nai,H0,H)
% This helper contains the logic for building the ATi matrix.
%
% INPUTS:
%   -> FkPHITTaSet: H*H cell array of reorderd (F^k*PHI)^{TT^a} 
%      matrices where k = col no. of cell array-row number
%   -> naiSet: 1*H vector of number of anticipated shocks used
%   -> nx: total number of endogenous variables
%   -> nai: total number of anticipated shocks used
%   -> H0: horizon up to which variables are being conditioned
%   -> H: forecast horizon
%
% OUTPUTS:
%   -> ATi: ATi matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
% The ATi matrix is an upper traingular block matrix, so the number of
% entries can be computed as an upper trianguler ones matrix multiplied by
% the number of anticipated shocks being used in each period, multiplied by
% the number of model variables.
nSparseEntries = nx*ones(1,H0)*triu(ones(H0,H))*naiSet';

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% LOOP OVER BLOCK ROWS & COLUMNS
% Each update of the sparse indices and rows incorporates anticipated shock
% instruments that can affect the variables in that particular period, so
% the block rows loop goes out as far as the last period in which
% conditioning information is being applied and the block columns loop goes
% all the way out to the end of the forecast horizon. Note that iterations
% in the inner loop are skipped if there are no instruments being used.
nEntriesInserted = 0;
hMatRowInds = (1:nx)';
for h = 1:H0
    hBlockRowInds = ((h-1)*nx+1:h*nx)';
    for s = h:H
        if naiSet(s) == 0
            continue
        end
        sBlockColInds = (1+sum(naiSet(1:s-1)):sum(naiSet(1:s)));
        sMatColInds = (1:naiSet(s));
        [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
            append_sparse_indices_and_values_with_new_block(...
            sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
            hBlockRowInds,sBlockColInds,FkPHITTaSet{h,s},hMatRowInds,...
            sMatColInds);
    end
end

%% BUILD SPARSE MATRIX
ATi = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nx*H0,nai);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING ATn MATRIX
function ATn = build_ATn_matrix(FkPHITTaSet,naiSet,nanSet,nx,nan,H0,H)
% This helper contains the logic for building the ATn matrix.
%
% INPUTS:
%   -> FkPHITTaSet: H*H cell array of reorderd (F^k*PHI)^{TT^a} 
%      matrices where k = col no. of cell array-row number
%   -> naiSet: 1*H vector of number of anticipated shocks used
%   -> nanSet: 1*H vector of number of anticipated shocks not used
%   -> nx: total number of endogenous variables
%   -> nan: total number of anticipated shocks not used
%   -> H0: horizon up to which variables are being conditioned
%   -> H: forecast horizon
%
% OUTPUTS:
%   -> ATn: ATn matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
% The logic of this is the same as for the ATi matrix in the sub-function
% above.
nSparseEntries = nx*ones(1,H0)*triu(ones(H0,H))*nanSet';

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% LOOP OVER BLOCK ROWS & COLUMNS
% The logic for this is the same as in the ATi matrix sub-function.
nEntriesInserted = 0;
hMatRowInds = (1:nx)';
for h = 1:H0
    hBlockRowInds = ((h-1)*nx+1:h*nx)';
    for s = h:H
        if nanSet(s) == 0
            continue
        end
        sBlockColInds = (1+sum(nanSet(1:s-1)):sum(nanSet(1:s)));
        sMatColInds = (naiSet(s)+1:naiSet(s)+nanSet(s));
        [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
            append_sparse_indices_and_values_with_new_block(...
            sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
            hBlockRowInds,sBlockColInds,FkPHITTaSet{h,s},hMatRowInds,...
            sMatColInds);
    end
end

%% BUILD SPARSE MATRIX
ATn = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nx*H0,nan);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING UTi MATRIX
function UTi = build_UTi_matrix(PHITTuSet,nuiSet,nx,nui,H0)
% This helper contains the logic for building the UTi matrix.
%
% INPUTS:
%   -> PHITTuSet: 1*H cell array of reorderd PHI^{TT^u} matrices 
%   -> nuiSet: 1*H vector of number of unanticipated shocks used
%   -> nx: total number of endogenous variables
%   -> nui: total number of unanticipated shocks used
%   -> H0: horizon up to which variables are being conditioned
%
% OUTPUTS:
%   -> UTi: UTi matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
% Note that the shock instruments are truncated at the final period in
% which variables are being conditioned. It is not possible to use
% unanticipated shocks as instruments beyond the final period of
% conditioning and this function will have thrown an error on any input in
% which this was attempted (so truncation here is both valid and safe).
nSparseEntries = nuiSet(1:H0)*nx*ones(H0,1);

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% LOOP OVER BLOCK ROWS & COLUMNS
% The UTi matrix is a block diagonal matrix - unanticipated shocks only
% affect the endogenous variables in the period in which they are realised
% (and then through the transition of the state as captured in the BBTT &
% BiT matrices).
nEntriesInserted = 0;
hMatRowInds = (1:nx)';
for h = 1:H0   
    if nuiSet(h) == 0
        continue
    end
    hBlockRowInds = ((h-1)*nx+1:h*nx)';
    hBlockColInds = (1+sum(nuiSet(1:h-1)):sum(nuiSet(1:h)));
    hMatColInds = (1:nuiSet(h));
    [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
        append_sparse_indices_and_values_with_new_block(...
        sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
        hBlockRowInds,hBlockColInds,PHITTuSet{h},hMatRowInds,hMatColInds);
end

%% BUILD SPARSE MATRICES
UTi = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nx*H0,nui);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING UTn MATRIX
function UTn = build_UTn_matrix(PHITTuSet,nuiSet,nunSet,nx,nun,H0)
% This helper contains the logic for building the UTn matrix.
%
% INPUTS:
%   -> PHITTuSet: 1*H cell array of reorderd PHI^{TT^u} matrices 
%   -> nuiSet: 1*H vector of number of unanticipated shocks used
%   -> nunSet: 1*H vector of number of unanticipated shocks not used
%   -> nx: total number of endogenous variables
%   -> nun: total number of unanticipated shocks not used
%   -> H0: horizon up to which variables are being conditioned
%
% OUTPUTS:
%   -> UTn: UTn matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
% The logic for this is the same as in the sub-function for UTi.
nSparseEntries = nunSet(1:H0)*nx*ones(H0,1);

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% LOOP OVER BLOCK ROWS & COLUMNS
% The logic for this is the same as in the sub-function for UTi.
nEntriesInserted = 0;
hMatRowInds = (1:nx)';
for h = 1:H0    
    if nunSet(h) == 0
        continue
    end
    hBlockRowInds = ((h-1)*nx+1:h*nx)';
    hBlockColInds = (1+sum(nunSet(1:h-1)):sum(nunSet(1:h)));
    hMatColInds = (nuiSet(h)+1:nuiSet(h)+nunSet(h));
    [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
        append_sparse_indices_and_values_with_new_block(...
        sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
        hBlockRowInds,hBlockColInds,PHITTuSet{h},hMatRowInds,hMatColInds);
end

%% BUILD SPARSE MATRIX
UTn = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nx*H0,nun);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING BiT MATRIX
function BiT = build_BiT_matrix(BTTset,nxiSet,nxi,nx,H0)
% This helper contains the logic for building the BiT matrix.
%
% INPUTS:
%   -> BTTset: 1*H cell array set of reorderd B matrices across time
%   -> nxiSet: 1*H vector of number of variables being conditioned
%   -> nxi: total number of variables being conditioned
%   -> nx: total number of variables
%   -> H0: horizon up to which variables are being conditioned
%
% OUTPUTS:
%   -> BiT: BiT matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
% The BiT matrix is a block diagonal matrix made up of nxi*nx elements.
nSparseEntries = nxi*nx;

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% LOOP OVER BLOCK ROWS & COLUMNS
nEntriesInserted = 0;
hMatColInds = (1:nx);
for h = 1:H0
    if nxiSet(h) == 0
        continue
    end
    hBlockRowInds = (sum(nxiSet(1:h-1))+1:sum(nxiSet(1:h)))';
    hBlockColInds = ((h-1)*nx+1:h*nx);
    hMatRowInds = (1:nxiSet(h))';
    [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
        append_sparse_indices_and_values_with_new_block(...
        sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
        hBlockRowInds,hBlockColInds,BTTset{h},hMatRowInds,hMatColInds);
end

%% BUILD SPARSE MATRICES
BiT = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nxi,nx*H0);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING Aii MATRIX
function Aii = build_Aii_matrix(FkPHITTaSet,nxiSet,naiSet,nxi,nai,H0,H)
% This helper contains the logic for building the ATi & ATn matrices.
%
% INPUTS:
%   -> FkPHITTaSet: H*H cell array of reorderd (F^k*PHI)^{TT^a} 
%      matrices where k = col no. of cell array-row number
%   -> nxiSet: 1*H vector of number of variables being conditioned
%   -> naiSet: 1*H vector of number of anticipated shocks used
%   -> nxi: total number of variables being conditioned
%   -> nai: total number of anticipated shocks used
%   -> H0: horizon up to which variables are being conditioned
%   -> H: forecast horizon
%
% OUTPUTS:
%   -> Aii: Aii matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
% The Aii is an upper traingular block matrix. There are triu(ones(H,H))*
% naiSet' block matrices in each row with the size of each of those blocks
% equal to nxiSet.
nSparseEntries = nxiSet*triu(ones(H,H))*naiSet';

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% LOOP OVER BLOCK ROWS & COLUMNS
% Note that iterations in both the inner and outer loop are skipped if no
% variables are being conditioned in that period (the outer block row loop)
% or if no anticipated shocks are being used in each future period (the
% inner block column loop).
nEntriesInserted = 0;
for h = 1:H0
    if nxiSet(h) == 0
        continue
    end
    hBlockRowInds = (sum(nxiSet(1:h-1))+1:sum(nxiSet(1:h)))';
    hMatRowInds = (1:nxiSet(h))';
    for s = h:H
        if naiSet(s) == 0
            continue
        end
        sBlockColInds = (sum(naiSet(1:s-1))+1:sum(naiSet(1:s)));
        sMatColInds = (1:naiSet(s));
        [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
            append_sparse_indices_and_values_with_new_block(...
            sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
            hBlockRowInds,sBlockColInds,FkPHITTaSet{h,s},hMatRowInds,...
            sMatColInds);
    end
end

%% BUILD SPARSE MATRICES
Aii = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nxi,nai);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING Ain MATRIX
function Ain = build_Ain_matrix(...
    FkPHITTaSet,nxiSet,naiSet,nanSet,nxi,nan,H0,H)
% This helper contains the logic for building the Ain matrix.
%
% INPUTS:
%   -> FkPHITTaSet: H*H cell array of reorderd (F^k*PHI)^{TT^a} 
%      matrices where k = col no. of cell array-row number
%   -> nxiSet: 1*H vector of number of variables being conditioned
%   -> naiSet: 1*H vector of number of anticipated shocks used
%   -> nanSet: 1*H vector of number of anticipated shocks not used
%   -> nxi: total number of variables being conditioned
%   -> nan: total number of anticipated shocks not used
%   -> H0: horizon up to which variables are being conditioned
%   -> H: forecast horizon
%
% OUTPUTS:
%   -> Ain: Ain matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
% The logic here is the same as in the Aii sub-function.
nSparseEntries = nxiSet*triu(ones(H,H))*nanSet';

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% LOOP OVER BLOCK ROWS & COLUMNS
% The logic here is the same as in the Aii sub-function.
nEntriesInserted = 0;
for h = 1:H0
    if nxiSet(h) == 0
        continue
    end
    hBlockRowInds = (sum(nxiSet(1:h-1))+1:sum(nxiSet(1:h)))';
    hMatRowInds = (1:nxiSet(h))';
    for s = h:H
        if nanSet(s) == 0
            continue
        end
        sBlockColInds = (sum(nanSet(1:s-1))+1:sum(nanSet(1:s)));
        sMatColInds = (naiSet(s)+1:naiSet(s)+nanSet(s));
        [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
            append_sparse_indices_and_values_with_new_block(...
            sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
            hBlockRowInds,sBlockColInds,FkPHITTaSet{h,s},hMatRowInds,...
            sMatColInds);
    end
end

%% BUILD SPARSE MATRIX
Ain = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nxi,nan);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING Uii MATRIX
function Uii = build_Uii_matrix(PHITTuSet,nxiSet,nuiSet,nxi,nui,H0)
% This helper contains the logic for building the Uii & Uin matrices.
%
% INPUTS:
%   -> PHITTuSet: 1*H cell array of reorderd PHI^{TT^u} matrices 
%   -> nxiSet: 1*H vector of number of variables being conditioned
%   -> nuiSet: 1*H vector of number of unanticipated shocks used
%   -> nxi: total number of variables being conditioned
%   -> nui: total number of unanticipated shocks used
%   -> H0: horizon up to which variables are being conditioned
%
% OUTPUTS:
%   -> Uii: Uii matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
nSparseEntries = nuiSet*nxiSet';

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% LOOP OVER BLOCK ROWS & COLUMNS
% The Uii matrix is block diagonal with each block containing relevant
% elements of the PHI matrix reordered consistent with the endogenous
% variaboles being conditioned and unanticipated shocks being used in the
% inversion.
nEntriesInserted = 0;
for h = 1:H0
    if nxiSet(h)==0 || nuiSet(h)==0
        continue
    end
    hBlockRowInds = (sum(nxiSet(1:h-1))+1:sum(nxiSet(1:h)))';
    hMatRowInds = (1:nxiSet(h))';
    hBlockColInds = (sum(nuiSet(1:h-1))+1:sum(nuiSet(1:h)));
    hMatColInds = (1:nuiSet(h));
    [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
        append_sparse_indices_and_values_with_new_block(...
        sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
        hBlockRowInds,hBlockColInds,PHITTuSet{h},hMatRowInds,...
        hMatColInds);
end

%% BUILD SPARSE MATRICES
Uii = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nxi,nui);

end

%% SUB-FUNCTION WITH LOGIC FOR BUILDING Uin MATRIX
function Uin = build_Uin_matrix(...
    PHITTuSet,nxiSet,nuiSet,nunSet,nxi,nun,H0)
% This helper contains the logic for building the Uin matrix.
%
% INPUTS:
%   -> PHITTuSet: 1*H cell array of reorderd PHI^{TT^u} matrices 
%   -> nxiSet: 1*H vector of number of variables being conditioned
%   -> nuiSet: 1*H vector of number of unanticipated shocks used
%   -> nunSet: 1*H vector of number of unanticipated shocks not used
%   -> nxi: total number of variables being conditioned
%   -> nun: total number of unanticipated shocks not used
%   -> H0: horizon up to which variables are being conditioned
%
% OUTPUTS:
%   -> Uin: Uin matrix

%% COMPUTE NUMBER OF SPARSE ENTRIES
nSparseEntries = nunSet*nxiSet';

%% INITIALISE SPARSE INDICES & VALUES
sparseRowInds = NaN*ones(nSparseEntries,1);
sparseColInds = sparseRowInds;
sparseVals = sparseRowInds;

%% LOOP OVER BLOCK ROWS & COLUMNS
% The logic is the same as in the sub-function for Uii 
nEntriesInserted = 0;
for h = 1:H0
    if nxiSet(h)==0 || nunSet(h)==0
        continue
    end
    hBlockRowInds = (sum(nxiSet(1:h-1))+1:sum(nxiSet(1:h)))';
    hMatRowInds = (1:nxiSet(h))';
    hBlockColInds = (sum(nunSet(1:h-1))+1:sum(nunSet(1:h)));
    hMatColInds = (nuiSet(h)+1:nuiSet(h)+nunSet(h));
    [sparseRowInds,sparseColInds,sparseVals,nEntriesInserted] = ...
        append_sparse_indices_and_values_with_new_block(...
        sparseRowInds,sparseColInds,sparseVals,nEntriesInserted,...
        hBlockRowInds,hBlockColInds,PHITTuSet{h},hMatRowInds,...
        hMatColInds);
end

%% BUILD SPARSE MATRIX
Uin = build_sparse_matrix_from_value_and_index_vectors(...
    sparseRowInds,sparseColInds,sparseVals,nxi,nun);

end

%% GENERIC HELPER FUNCTION TO BUILD UP SPARSE INDICES & VALUES VECTORS
function [sparseRowInds,sparseColInds,sparseVals,nEntriesNowInserted] = ...
    append_sparse_indices_and_values_with_new_block(...
    sparseRowInds,sparseColInds,sparseVals,nEntriesAlreadyInserted,...
    blockRowInds,blockColInds,modelMat,modelMatRowInds,modelMatColInds)
% This helper contains logic for the updating of sparse indices & values.
%
% INPUTS:
%   -> sparseRowInds: complete column vector of sparse row indices
%   -> sparseColInds: complete column vector of sparse column indices
%   -> sparseVals: complete column vector of corresponding values
%   -> nEntriesAlreadyInserted: number of entries already inserted
%      (non-NaN)
%   -> blockRowInds: row indices of elements to be inserted in the matrix
%      being built
%   -> blockRowInds: column indices of elements to be inserted in the 
%      matrix being built
%   -> modelMat: model solution matrix being used to update the values
%   -> modelMatRowInds: row indices of the block of the matrix to use
%   -> modelMatColInds: column indices of the block of the matrix to use
%
% OUTPUTS:
%   -> sparseRowInds: updated column vector of sparse row indices
%   -> sparseColInds: updated column vector of sparse column indices
%   -> sparseVals: updated column vector of corresponding values
%   -> nEntriesNowInserted: updated number of entries inserted

%% EXTRACT RELEVANT BLOCK OF MODEL MATRIX
modelMatBlock = modelMat(modelMatRowInds,modelMatColInds);
nRows = size(modelMatBlock,1);
nCols = size(modelMatBlock,2);
nElements = nRows*nCols;

%% COMPUTE BLOCK ROW AND COLUMN INDICES TO APPEND TO SPARSE INDEX VECTORS
% This expands the row indices across the columns and the column indices
% across the rows. The vectorisation of these matrices below ensures that
% the complete set of row and column indices describing where the model
% solution matrix values "live" in the sparse matrix being built are in the
% correct order (i.e. line up with the vectorised set of model solution
% values).
blockRowIndsAcrossCols = blockRowInds*ones(1,nCols);
blockColIndsAcrossRows = ones(nRows,1)*blockColInds;

%% COMPUTE SPARSE INDICES
nEntriesNowInserted = nEntriesAlreadyInserted+nElements;
blockSparseInds = (nEntriesAlreadyInserted+1:nEntriesNowInserted)';

%% INSERT THE INDEX NUMBERS AND VALUES
% This updates the sparse row, column and values vectors being built. Note
% that prior to this cell, the sparse vectors contain NaN elements from
% nEntriesAlreadyInserted+1 to nTotalEntries. This cell updates a block of
% these vectors. As an example of the logic of the creation of sparse row,
% column and value indices, consider how to create a sparse version of the
% matrix [1 2 3; 4 5 6]. In this example, the sparse vectors used to build
% up the sparse version of the matrix have 6 elements in total. The input
% to this function would be 3 NaN*ones(6,1) vectors for the sparse indices
% and values vectors and a scalar for the number of entries already 
% inserted equal to 0. The input blockRowInds would be [1;2] and the input 
% blockColInds would be [1 2 3]. The modelMat input would just be this
% matrix and the modelMatRowInds/modelMatColInds would be the same as the
% block row index vectors if we eere creating the sparse vectors in one
% call. The resulting sparse vectors would look like the following:
% sparseRowInds = [1;2;1;2;1;2], sparseRowInds = [1;1;2;2;3;3] & 
% sparseVals = [1;4;2;5;3;6]. A call to the MATLAB sparse function with
% those inputs would create a sparse equivalent to the original "full"
% matrix.
blockRowIndsAcrossColsVec = blockRowIndsAcrossCols(:);
blockColIndsAcrossRowsVec = blockColIndsAcrossRows(:);
modelMatBlockVec = modelMatBlock(:);
sparseRowInds(blockSparseInds) = blockRowIndsAcrossColsVec;
sparseColInds(blockSparseInds) = blockColIndsAcrossRowsVec;
sparseVals(blockSparseInds) = modelMatBlockVec;

end

%% HELPER FUNCTION TO BUILD SPARSE MATRIX FROM VECTORS OF VALS & INDS
function sparseMat = build_sparse_matrix_from_value_and_index_vectors(...
    rowIndsVec,colIndsVec,valsVec,nRows,nCols)
% This helper builds a sparse matrix from vectors of indices and values.
%
% INPUTS:
%   -> rowIndsVec: column vector of row indices
%   -> colIndsVec: column vector of column indices
%   -> valsVec: column vector of values
%   -> nRows: total number of rows in the matrix
%   -> nCols: total number of columns in the matrix
%
% OUTPUTS:
%   -> sparseMat: sparse matrix

%% COMPUTE NON-ZERO INDICES
% Note that this step allows the removal of any elements in the values
% vector (and corresponding index vectors) that are zero. This can
% substantially improve the performance of the call to the sparse function
% if a non-engligible fraction of the values are zeros (which begs the
% question of why MATLAB don't embed thus logic in sparse itself). The step
% is required because the model sulution matrices used to build up the
% sparse values vectors are "full" matrices and may contain a reasonably
% large number of zero values. I could not think of an efficient
% implementation that would remove the zeros at each stage of the building
% process (given that the call to find here is pretty quick even if the
% vectors are large).
nonZeroValInds = find(valsVec);

%% CREATE SPARSE MATRIX
sparseMat = sparse(rowIndsVec(nonZeroValInds),...
    colIndsVec(nonZeroValInds),valsVec(nonZeroValInds),nRows,nCols);

end

%% FUNCTION TO STACK CONDITIONING PATHS AND SHOCKS
function [xibar,zibar,anbar,unbar,xiWeightMat,ziWeightMat] = ...
    stack_conditioning_paths_shocks_and_weights(...
    xibarSet,anbarSet,unbarSet,aibarSet,uibarSet,...
    xiWeightSet,aiWeightSet,uiWeightSet)
% This helper stacks known values across the whole inversion horizon.
%
% INPUTS:
%   -> xibarSet: 1*H cell of conditioning path target values
%   -> anbarSet: 1*H cell of existing values for the anticipated shocks
%      not being used
%   -> unbarSet: 1*H cell of existing values for the unanticipated shocks
%      not being used
%   -> aibarSet: 1*H cell of existing values for the anticipated shocks
%      being used
%   -> uibarSet: 1*H cell of existing values for the unanticipated shocks
%      being used
%   -> xiWeightSet: 1*HY cell array of weights to apply to conditioing
%      paths
%   -> aiWeightSet: 1*HY cell array of weights to apply to anticipated
%      shocks
%   -> uiWeightSet: 1*HY cell array of weights to apply to unanticipated
%      shocks
%
% OUTPUTS:
%   -> xibar: nxi*1 vector of all conditioing path values
%   -> zibar: (nai+nui)*1 vector of existing values for all shocks being
%      used in the inversion
%   -> anbarSet: nan*1 vector of values for all anticipated shocks not 
%      being used
%   -> unbarSet: nan*1 vector of values for all unanticipated shocks not 
%      being used
%   -> xiWeightMat: nxi*nxi diagonal matrix of weights to apply to
%      conditioing paths in case of under-identified inversion
%   -> ziWeightMat: nzi*nzi diagonal matrix of weights to apply to
%      shocks in case of over-identified inversion

%% STACK ALL VALUES TOGETHER
xibar = cell2mat(xibarSet(:));
aibar = cell2mat(aibarSet(:));
uibar = cell2mat(uibarSet(:));
anbar = cell2mat(anbarSet(:));
unbar = cell2mat(unbarSet(:));
xiWeights = cell2mat(xiWeightSet(:));
aiWeights = cell2mat(aiWeightSet(:));
uiWeights = cell2mat(uiWeightSet(:));

%% STACK EXISTING VALUES FOR ANTICIPATED & UNANTICIPATED SHOCKS TOGETHER
zibar = [aibar;uibar];
ziWeights = [aiWeights;uiWeights];

%% CONVERT WEIGHTS TO DIAGONAL MATRIX
xiWeightMat = diag(xiWeights);
ziWeightMat = diag(ziWeights);

end

%% FUNCTION TO COMPUTE REORDERED MODEL SOLUTION MATRICES
function [BTTset,FkPHITTaSet,PHITTuSet] = ...
    reorder_model_solution_matrices(B,F,PHI,TxSet,TaSet,TuSet,...
    TxIsIdenticalToTxLag,TaIsIdenticalToTaLag,TuIsIdenticalToTuLag,H,nx)
% This helper reorders the solution matrices consistent with the derivation
% It uses the reorder operators to reorder the model solution to be
% consistent with the inversion convention that model variables are
% reordered with variables conditioned first, then variables not 
% conditioned and that both anticipated & unanticipated shocks are
% separately reordered so that the shocks being used to achieve the
% inversion are ordered first.
%
% INPUTS:
%   -> B: backward loadings in the model solution
%   -> F: forward loadings for anticipated shocks in the model solution
%   -> PHI: loadings on the shocks in the model solution
%   -> TxSet: reorder operators for model variables in each period
%   -> TaSet: reorder operators for anticipated shocks in each period
%   -> TuSet: reorder operators for unanticipated shocks in each period
%   -> TxIsIdenticalToTxLag: vec of logicals - true if TxSet(h)==TxSet(h-1)
%   -> TaIsIdenticalToTaLag: vec of logicals - true if TaSet(h)==TaSet(h-1)
%   -> TuIsIdenticalToTuLag: vec of logicals - true if TuSet(h)==TuSet(h-1)
%   -> H: forecast horizon
%   -> nx: number of model variables in the model
%
% OUTPUTS:
%   -> BTTset: H*1 cell array of reordered solution backward loadings for 
%      each period
%   -> FkPHITTaSet: H*H upper diagonal cell array of forward loadings on 
%      anticipated shocks, where k = column number - row number
%   -> PHITTuSet: H*1 cell array of reordered unanticipated shock loadings
%      for each period

%% SETUP THE OUTPUT
% Setup the output as cell array row vectors with H elements.
BTTset = cell(H,1);
FkPHITTaSet = cell(H,H);
PHITTuSet = cell(H,1);
FTset = cell(H,1);

%% REORDER THE SOLUTION
% Reorder the backward, forward and shocks loadings in the model solution
% to be consistent with the reordered variables and shocks as given by the
% reorder operators. The code takes advantage of the fact that the 
% reordered matrices may be identical from period to period if the 
% variables being conditioned and the shocks being used are the same. Note
% the assumption that the model variables are ordered in the same way as in
% the model in the initial conditions. Note also that the reordered 
% loadings on the anticipated shocks form an upper triangular cell array.
for h = 1:H
    if h == 1
        BTTset{h} = TxSet{h}*B*eye(nx,nx);
    else
        if TxIsIdenticalToTxLag(h) && TxIsIdenticalToTxLag(h-1)
            BTTset{h} = BTTset{h-1};
        else
            BTTset{h} = TxSet{h}*B*(TxSet{h-1}');
        end
    end
    if h>1 && TxIsIdenticalToTxLag(h)
        FTset{h} = FTset{h-1};
    else
        FTset{h} = TxSet{h}*F*(TxSet{h}');
    end
    if h>1 && TxIsIdenticalToTxLag(h) && TuIsIdenticalToTuLag(h)
        PHITTuSet{h} = PHITTuSet{h-1};
    else
        PHITTuSet{h} = TxSet{h}*PHI*(TuSet{h}');
    end
    for k = 0:H-h
        if h>1 && TxIsIdenticalToTxLag(h) && TaIsIdenticalToTaLag(h+k)
            FkPHITTaSet{h,h+k} = FkPHITTaSet{h-1,h+k-1};
        else
            PHITTa = TxSet{h}*PHI*(TaSet{h+k}');
            FkPHITTaSet{h,h+k} = (FTset{h}^k)*PHITTa;
        end
    end
end

end

%% FUNCTION TO COMPUTE REORDER OPERATORS
function [TxSet,TaSet,TuSet,TxIsIdenticalToTxLag,TaIsIdenticalToTaLag,...
    TuIsIdenticalToTuLag] = compute_reorder_operators(...
    nx,nxiSet,xiIndSet,nz,naiSet,aiIndSet,nanSet,anIndSet,nuiSet,...
    uiIndSet,nunSet,unIndSet,H)
% This helper computes the reorder operators to reorder the model.
% The reorder operators are consistent with the inversion convention that 
% model variables are reordered with variables being conditioned first, 
% then variables not being conditioned with shocks ordered by shocks being
% used in the inversion, then shocks not used.
%
% INPUTS:
%   -> nx: number of endogenous model variables in the model
%   -> nxiSet: 1*H vector of numeric values for number of variables being
%      conditioned in each period
%   -> xiIndSet: 1*H cell of index identifiers for the anticipated paths
%   -> nz: number of shocks in the model
%   -> naiSet: 1*H vector of numeric values for number of anticipated
%      shocks being used in each period
%   -> aiIndSet: 1*H cell of index identifiers for the anticipated shocks
%      being used
%   -> nanSet: 1*H vector of numeric values for number of anticipated
%      shocks not being used in each period
%   -> anIndSet: 1*H cell of index identifiers for the anticipated shocks
%      not being used
%   -> nuiSet: 1*H vector of numeric values for number of unanticipated
%      shocks being used in each period
%   -> uiIndSet: 1*H cell of index identifiers for the unanticipated shocks
%      being used
%   -> nunSet: 1*H vector of numeric values for number of unanticipated
%      shocks not being used in each period
%   -> unIndSet: 1*H cell of index identifiers for the unanticipated shocks
%      not being used
%   -> H: forecast horizon
%
% OUTPUTS:
%   -> TxSet: reorder operators for model variables in each period
%   -> TaSet: reorder operators for anticipated shocks in each period
%   -> TuSet: reorder operators for unanticipated shocks in each period
%   -> TxIsIdenticalToTxLag: vec of logicals - true if TxSet(h)==TxSet(h-1)
%   -> TaIsIdenticalToTaLag: vec of logicals - true if TaSet(h)==TaSet(h-1)
%   -> TuIsIdenticalToTuLag: vec of logicals - true if TuSet(h)==TuSet(h-1)

%% SETUP OUTPUT
% Setup the reorder operator outputs as row cell arrays across each of the
% periods in the inversion and some logicals that can be used to infer if
% the reorder operators were the same in the previous period.
TxSet = cell(1,H);
TaSet = cell(1,H);
TuSet = cell(1,H);
TxIsIdenticalToTxLag = false(1,H);
TaIsIdenticalToTaLag = false(1,H);
TuIsIdenticalToTuLag = false(1,H);

%% SETUP SOME MODEL ORDERED INDICES
% Setup some model ordered indices to be used below.
xInd = (1:nx)';
zInd = (1:nz)';
ixVec = xInd;
iaVec = zInd;
iuVec = zInd;

%% LOOP THROW INVERSION PERIODS
% In each iteration setup appropriately sized reordering oeprators. Setup a
% vector that will be used to create the reorder operators. For model
% variables, the convention used is that variables being conditioned are 
% ordered first, then variables not fixed (ie those allowed to respond to 
% the inversion). For each type of shock, the convention is that the shocks
% being used to achieve the inversion are ordered before those held fixed 
% (not used) in the inversion. The vectors are set such that the indices 
% of the vector point to the original model ordering of variables and 
% shocks, while the values dictate the new ordering.
for h = 1:H
    TxSet{h} = zeros(nx,nx);
    TaSet{h} = zeros(nz,nz);
    TuSet{h} = zeros(nz,nz);
    jxVec = zeros(nx,1);
    jaVec = zeros(nz,1);
    juVec = zeros(nz,1);
    for ixi = 1:nxiSet(h)
        jxVec(xiIndSet{h}(ixi)) = ixi;
    end
    for iai = 1:naiSet(h)
        jaVec(aiIndSet{h}(iai)) = iai;
    end
    for ian = 1:nanSet(h)
        jaVec(anIndSet{h}(ian)) = naiSet(h)+ian;
    end
    for iui = 1:nuiSet(h)
        juVec(uiIndSet{h}(iui)) = iui;
    end
    for iun = 1:nunSet(h)
        juVec(unIndSet{h}(iun)) = nuiSet(h)+iun;
    end
    jxVec(jxVec==0) = (nxiSet(h)+1:nx)';
    TxSet{h}((ixVec-1)*nx+jxVec) = 1;
    TaSet{h}((iaVec-1)*nz+jaVec) = 1;
    TuSet{h}((iuVec-1)*nz+juVec) = 1;
    if h == 1
        if nxiSet(h) == 0
            TxIsIdenticalToTxLag(h) = true;
        end
        if naiSet(h) == 0
            TaIsIdenticalToTaLag(h) = true;
        end
        if nuiSet(h) == 0
            TuIsIdenticalToTuLag(h) = true;
        end
    else
        if isequal(xiIndSet{h},xiIndSet{h-1})
            TxIsIdenticalToTxLag(h) = true;
        end
        if isequal(aiIndSet{h},aiIndSet{h-1})
            TaIsIdenticalToTaLag(h) = true;
        end
        if isequal(uiIndSet{h},uiIndSet{h-1})
            TuIsIdenticalToTuLag(h) = true;
        end
    end
end

end

%% FUNCTION TO UNPACK INVERSION INSTRUCTIONS OBJECT
function [nx,nz,H,inversionType,inversionIncludesWeights,xibarSet,...
    xiWeightSet,xiIndSet,nxiSet,nxi,aibarSet,aiWeightSet,aiIndSet,...
    naiSet,nai,anbarSet,anIndSet,nanSet,nan,uibarSet,uiWeightSet,...
    uiIndSet,nuiSet,nui,unbarSet,unIndSet,nunSet,nun,...
    overIdentificationIsMinimumVariance] = ...
    unpack_inversion_instructions_object(InversionObj)
% This helper checks & unpacks the inversion instructions object.
% It is in effect a config that contains the location of all the objects
% necessary for the inversion. 
%
% INPUTS:
%   -> InversionObj: inversion instructions object (see
%      Inversion.Instructions class for details)
%
% OUTPUTS:
%   -> nx: number of endogenous variables
%   -> nz: number of shocks
%   -> H: forecast horizon
%   -> inversionType: "exactly-identified", "over-identified" or "under-
%      idetified"
%   -> inversionIncludesWeights: true/false
%   -> xibarSet: 1*H cell of conditioning path target values
%   -> xiWeightSet: 1*H cell of weights for the target values
%   -> xiIndSet: 1*H cell of index identifiers for the anticipated paths
%   -> nxiSet: 1*H vector of numeric values for number of variables being
%      conditioned in each period
%   -> nxi: total number of variables being conditioned in all periods
%   -> aibarSet: 1*H cell of existing values for the anticipated shocks
%      being used
%   -> aiWeightSet: 1*H cell of weights for the anticipated shocks in the
%      inversion
%   -> aiIndSet: 1*H cell of index identifiers for the anticipated shocks
%      being used
%   -> naiSet: 1*H vector of numeric values for number of anticipated
%      shocks being used in each period
%   -> nai: total number of anticipated shocks being used
%   -> anbarSet: 1*H cell of existing values for the anticipated shocks
%      not being used
%   -> anIndSet: 1*H cell of index identifiers for the anticipated shocks
%      not being used
%   -> nanSet: 1*H vector of numeric values for number of anticipated
%      shocks not being used in each period
%   -> nan: total number of anticipated shocks not being used
%   -> uibarSet: 1*H cell of existing values for the unanticipated shocks
%      being used
%   -> uiWeightSet: 1*H cell of weights for the unanticipated shocks in the
%      inversion
%   -> uiIndSet: 1*H cell of index identifiers for the unanticipated shocks
%      being used
%   -> nuiSet: 1*H vector of numeric values for number of unanticipated
%      shocks being used in each period
%   -> nui: total number of unanticipated shocks being used
%   -> unbarSet: 1*H cell of existing values for the unanticipated shocks
%      not being used
%   -> unIndSet: 1*H cell of index identifiers for the unanticipated shocks
%      not being used
%   -> nunSet: 1*H vector of numeric values for number of unanticipated
%      shocks not being used in each period
%   -> nun: total number of unanticipated shocks not being used
%   -> overIdentificationIsMinimumVariance: true/false

%% VALIDATE INVERSION OBJECT
InversionObj.validate();

%% UNPACK GENERAL INFORMATION ABOUT THE INVERSION
nx = InversionObj.nEndogVars;
nz = InversionObj.nShocks;
H = InversionObj.horizon;
inversionType = InversionObj.inversionType;
inversionIncludesWeights = InversionObj.inversionIncludesWeights;

%% UNPACK INFORMATION ABOUT TARGETS
xibarSet = InversionObj.Targets.values;
xiWeightSet = InversionObj.Targets.weights;
xiIndSet = InversionObj.Targets.indices;
nxiSet = InversionObj.Targets.numberPerPeriod;
nxi = InversionObj.Targets.totalNumber;

%% UNPACK INFORMATION ABOUT THE ANTICIPATED INSTRUMENTS & NON-INSTRUMENTS
aibarSet = InversionObj.Instruments.Anticipated.values;
aiWeightSet = InversionObj.Instruments.Anticipated.weights;
aiIndSet = InversionObj.Instruments.Anticipated.indices;
naiSet = InversionObj.Instruments.Anticipated.numberPerPeriod;
nai = InversionObj.Instruments.Anticipated.totalNumber;
anbarSet = InversionObj.Instruments.Anticipated.reflectedValues;
anIndSet = InversionObj.Instruments.Anticipated.reflectedIndices;
nanSet = InversionObj.Instruments.Anticipated.reflectedNumberPerPeriod;
nan = InversionObj.Instruments.Anticipated.reflectedTotalNumber;

%% UNPACK INFORMATION ABOUT THE UNANTICIPATED INSTRUMENTS & NON-INSTRUMENTS
uibarSet = InversionObj.Instruments.Unanticipated.values;
uiWeightSet = InversionObj.Instruments.Unanticipated.weights;
uiIndSet = InversionObj.Instruments.Unanticipated.indices;
nuiSet = InversionObj.Instruments.Unanticipated.numberPerPeriod;
nui = InversionObj.Instruments.Unanticipated.totalNumber;
unbarSet = InversionObj.Instruments.Unanticipated.reflectedValues;
unIndSet = InversionObj.Instruments.Unanticipated.reflectedIndices;
nunSet = InversionObj.Instruments.Unanticipated.reflectedNumberPerPeriod;
nun = InversionObj.Instruments.Unanticipated.reflectedTotalNumber;

%% UNPACK INFORMATION ABOUT THE OPTIONS
overIdentificationIsMinimumVariance = ...
    InversionObj.Options.overIdentificationIsMinimumVariance;

end