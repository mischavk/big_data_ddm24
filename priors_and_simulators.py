import ctypes
from numba.extending import get_cython_function_address
from simulators import levy
import sys
import scipy
from numba import njit, prange
import numpy as np
import scipy.stats as stats

# Get a pointer to the C function levy.c
addr_levy= get_cython_function_address("levy", "levy_trial")
functype = ctypes.CFUNCTYPE(ctypes.c_double, ctypes.c_double, ctypes.c_double, 
                            ctypes.c_double, ctypes.c_double, ctypes.c_double,
                            ctypes.c_double, ctypes.c_double, ctypes.c_double, 
                            ctypes.c_double, ctypes.c_double, ctypes.c_int)
levy_trial = functype(addr_levy)

RNG = np.random.default_rng(2023)


def simple_ddm_prior_fun():
    """
    Samples from the prior (once).
    ----------

    # Prior ranges for the simulator 
    # v1 & v2 ~ G(2.0, 1.0)
    # a1 & a2 ~ G(6.0, 0.3)
    # ndt_plus ~ G(3.0, 0.15)
    # ndt_minus ~ G(3.0, 0.5)
    
    """

    drifts = RNG.gamma(2.0, 1.0, size=2)

    thresholds = RNG.gamma(6.0, 0.3, size=2)

    ndt_plus =  RNG.gamma(3.0, 0.15)
    ndt_minus = RNG.gamma(3.0, 0.5)
   
    return np.hstack((drifts, thresholds, ndt_plus, ndt_minus))


def levy_prior_fun():
    """
    Samples from the prior (once).
    ----------

    # Prior ranges for the simulator 
    # v1 & v2 ~ G(2.0, 1.0)
    # a1 & a2 ~ G(6.0, 0.3)
    # ndt_plus ~ G(3.0, 0.15)
    # ndt_minus ~ G(3.0, 0.5)
    # alpha ~ truncated N(1.5, 0.5, a=1.0, b=2.0)
    
    """

    drifts = RNG.gamma(2.0, 1.0, size=2)

    thresholds = RNG.gamma(6.0, 0.3, size=2)

    ndt_plus =  RNG.gamma(3.0, 0.15)
    ndt_minus = RNG.gamma(3.0, 0.5)
    
    lower, upper = 1.0, 2.0
    mu, sigma = 1.5, 0.5
    
    alpha = stats.truncnorm.rvs((lower - mu) / sigma, (upper - mu) / sigma, loc=mu, scale=sigma)
   
    return np.hstack((drifts, thresholds, ndt_plus, ndt_minus, alpha))

def st0_ddm_prior_fun():
    """
    Samples from the prior (once).
    ----------

    # Prior ranges for the simulator 
    # v1 & v2 ~ G(2.0, 1.0)
    # a1 & a2 ~ G(6.0, 0.3)
    # ndt_plus ~ G(3.0, 0.15)
    # ndt_minus ~ G(3.0, 0.5)
    # st0 ~ U(0, ndt_plus/2)
    
    """

    drifts = RNG.gamma(2.0, 1.0, size=2)

    thresholds = RNG.gamma(6.0, 0.3, size=2)

    ndt_plus =  RNG.gamma(3.0, 0.15)
    ndt_minus = RNG.gamma(3.0, 0.5)
    
    st0 = RNG.uniform(0.0, ndt_plus/2)
   
    return np.hstack((drifts, thresholds, ndt_plus, ndt_minus, st0))

def sv_st0_ddm_prior_fun():
    """
    Samples from the prior (once).
    ----------

    # Prior ranges for the simulator 
    # v1 & v2 ~ G(2.0, 1.0)
    # a1 & a2 ~ G(6.0, 0.3)
    # ndt_plus ~ G(3.0, 0.15)
    # ndt_minus ~ G(3.0, 0.5)
    # st0 ~ U(0, ndt_plus/2)
    # sv ~ halfN(0.0, 1.0)
    
    """

    drifts = RNG.gamma(2.0, 1.0, size=2)

    thresholds = RNG.gamma(6.0, 0.3, size=2)

    ndt_plus =  RNG.gamma(3.0, 0.15)
    ndt_minus = RNG.gamma(3.0, 0.5)
    st0 = RNG.uniform(0.0, ndt_plus/2)
    sv = stats.halfnorm.rvs(loc = 0, scale = 1, size=1)
   
    return np.hstack((drifts, thresholds, ndt_plus, ndt_minus, st0, sv))

def gsr_ddm_prior_fun():
    """
    Samples from the prior (once).
    ----------

    # Prior ranges for the simulator 

    # similarity (gamma) ~ (Beta(3.0, 3.0) - 0.5) *2
    # v1 & v2 ~ G(2.0, 1.0)
    # a1 & a2 ~ G(6.0, 0.3)
    # ndt_plus ~ G(3.0, 0.15)
    # ndt_minus ~ G(3.0, 0.5)
    # st0 ~ U(0, ndt_plus/2)
    # sv ~ halfN(0.0, 1.0)
    
    """
    similarity = (RNG.beta(3.0, 3.0) - 0.5 ) * 2
    
    drift_word = RNG.gamma(2.0, 1.0)
    drift_picture = RNG.gamma(2.0, 1.0)

    thresholds = RNG.gamma(6.0, 0.3, size=2)

    ndt_plus =  RNG.gamma(3.0, 0.15)
    ndt_minus = RNG.gamma(3.0, 0.5)
    
    sndt = RNG.uniform(0.0, ndt_plus/2)
    sv = stats.halfnorm.rvs(loc = 0, scale = 1, size=1)
   
    return np.hstack((similarity, drift_word, drift_picture, thresholds, ndt_plus, ndt_minus, sv, sndt))


#Simple DDM simulator
# For a single trial
@njit 
def simple_ddm_iat_diffusion_trial(v, a, ndtcorrect, ndterror):
    """Simulates a trial from the diffusion model."""

    n_steps = 0.
    x = a * 0.5
    dt = 0.001
    max_steps = 10000

    # Simulate a single DM path
    while (x > 0 and x < a and n_steps < max_steps):

        # DDM equation
        x += v*dt + np.sqrt(dt) * np.random.normal()

        # Increment step
        n_steps += 1.0

    rt = n_steps * dt
    return rt + ndtcorrect if x > 0. else -rt - ndterror # use different ndts for correct and error responses


#Simple DDM simulator
# For an entire subject
@njit
def simple_ddm_simulator_fun(theta):
    """Simulates data from a single subject in an IAT experiment."""
    num_obs = 120
    
    obs_per_condition = 60
    
    condition_type = np.arange(2)
    condition_type = np.repeat(condition_type, obs_per_condition)
    
    # code stimulus types, picture == 1
    stimulus_type = np.concatenate ((np.zeros(30), np.ones(30),# condition 1: congruent
                                       np.zeros(30), np.ones(30)) ) # condition 2: incongruent
            
    v = theta[0:2]
    a = theta[2:4]

    out = np.zeros(num_obs)
    
    for n in range(num_obs):
        out[n] = simple_ddm_iat_diffusion_trial(v[condition_type[n]], a[condition_type[n]], theta[4], theta[5])
        # mark too slow or too fast trials with zero
        if abs(out[n]) > 10.0:
            out[n] = 0
        if abs(out[n]) < 0.2:
            out[n] = 0
    
    missings = np.expand_dims(np.zeros((out.shape[0])),1)
    missings [out==0] = 1
    
    out = np.expand_dims(out, 1)
    condition_type = np.expand_dims(condition_type, 1)
    stimulus_type = np.expand_dims(stimulus_type, 1)
    out = np.concatenate((out, missings, condition_type, stimulus_type), axis=1) 
    
    return out

    
    
# st0 DDM simulator
# For a single trial
@njit 
def st0_ddm_iat_diffusion_trial(v, a, ndtcorrect, ndterror, sndt):
    """Simulates a trial from the diffusion model."""

    n_steps = 0.
    x = a * 0.5
    dt = 0.001
    max_steps = 10000
    ndtcorrect = ndtcorrect - 0.5*sndt + sndt * np.random.uniform(0, 1)

    # Simulate a single DM path
    while (x > 0 and x < a and n_steps < max_steps):

        # DDM equation
        x += v*dt + np.sqrt(dt) * np.random.normal()

        # Increment step
        n_steps += 1.0

    rt = n_steps * dt
    return rt + ndtcorrect if x > 0. else -rt - ndterror # use different ndts for correct and error responses

# For an entire subject
@njit
def st0_ddm_simulator_fun(theta):
    """Simulates data from a single subject in an IAT experiment."""
    
    num_obs = 120
            
    obs_per_condition = 60
    
    condition_type = np.arange(2)
    condition_type = np.repeat(condition_type, obs_per_condition)
    
    # code stimulus types, picture == 1
    stimulus_type = np.concatenate ((np.zeros(30), np.ones(30),# condition 1: congruent
                                       np.zeros(30), np.ones(30)) ) # condition 2: incongruent
    
    v = theta[0:2]
    a = theta[2:4]

    out = np.zeros(num_obs)
    
    for n in range(num_obs):
        out[n] = st0_ddm_iat_diffusion_trial(v[condition_type[n]], a[condition_type[n]],
                                             theta[4], theta[5], theta[6])
        # mark too slow or too fast trials with zero
        if abs(out[n]) > 10.0:
            out[n] = 0
        if abs(out[n]) < 0.2:
            out[n] = 0
            

    missings = np.expand_dims(np.zeros((out.shape[0])),1)
    missings [out==0] = 1
    
    out = np.expand_dims(out, 1)
    condition_type = np.expand_dims(condition_type, 1)
    stimulus_type = np.expand_dims(stimulus_type, 1)
    out = np.concatenate((out, missings, condition_type, stimulus_type), axis=1) 
    
    return out
    
#sv st0 ddm simulator
# For a single trial
@njit 
def sv_st0_ddm_iat_diffusion_trial(v, a, ndtcorrect, ndterror, sndt, sv):
    """Simulates a trial from the diffusion model."""

    n_steps = 0.
    x = a * 0.5
    dt = 0.001
    max_steps = 10000
    ndtcorrect = ndtcorrect - 0.5*sndt + sndt * np.random.uniform(0, 1)
    v = v + sv * np.random.normal()

    # Simulate a single DM path
    while (x > 0 and x < a and n_steps < max_steps):

        # DDM equation
        x += v*dt + np.sqrt(dt) * np.random.normal()

        # Increment step
        n_steps += 1.0

    rt = n_steps * dt
    return rt + ndtcorrect if x > 0. else -rt - ndterror # use different ndts for correct and error responses

# For an entire subject
@njit
def sv_st0_ddm_simulator_fun(theta):
    """Simulates data from a single subject in an IAT experiment."""
    num_obs = 120
           
    obs_per_condition = 60
    
    condition_type = np.arange(2)
    condition_type = np.repeat(condition_type, obs_per_condition)
    
    # code stimulus types, picture == 1
    stimulus_type = np.concatenate ((np.zeros(30), np.ones(30),# condition 1: congruent
                                       np.zeros(30), np.ones(30)) ) # condition 2: incongruent
    
    v = theta[0:2]
    a = theta[2:4]

    out = np.zeros(num_obs)
    
    for n in range(num_obs):
        out[n] = sv_st0_ddm_iat_diffusion_trial(v[condition_type[n]], a[condition_type[n]],
                                     theta[4], theta[5], theta[6], theta[7])
        # mark too slow or too fast trials with zero
        if abs(out[n]) > 10.0:
            out[n] = 0
        if abs(out[n]) < 0.2:
            out[n] = 0
            
       

    missings = np.expand_dims(np.zeros((out.shape[0])),1)
    missings [out==0] = 1
    
    out = np.expand_dims(out, 1)
    condition_type = np.expand_dims(condition_type, 1)
    stimulus_type = np.expand_dims(stimulus_type, 1)
    out = np.concatenate((out, missings, condition_type, stimulus_type), axis=1) 
    
    return out

# levy simulator
# For an entire subject

@njit 
def levy_simulator_fun(theta):
    """Simulates data from a single subject in an IAT experiment."""
    num_obs = 120  
        
    obs_per_condition = 60
    
    condition_type = np.arange(2)
    condition_type = np.repeat(condition_type, obs_per_condition)
    
    # code stimulus types, picture == 1
    stimulus_type = np.concatenate ((np.zeros(30), np.ones(30),# condition 1: congruent
                                       np.zeros(30), np.ones(30)) ) # condition 2: incongruent
    
    v = theta[0:2]
    a = theta[2:4]

    out = np.zeros(num_obs)
    
    for n in range(num_obs):
        out[n] = levy_trial(v[condition_type[n]], 
                                 0, 
                                 0.5, 
                                 0, 
                                 a[condition_type[n]], 
                                 theta[4], 
                                 theta[5],
                                 0, 
                                 theta[6],
                                 0.001, 10000)
                # mark too slow or too fast trials with zero
        if abs(out[n]) > 10.0:
            out[n] = 0
        if abs(out[n]) < 0.2:
            out[n] = 0

    missings = np.expand_dims(np.zeros((out.shape[0])),1)
    missings [out==0] = 1
    
    out = np.expand_dims(out, 1)
    condition_type = np.expand_dims(condition_type, 1)
    stimulus_type = np.expand_dims(stimulus_type, 1)
    out = np.concatenate((out, missings, condition_type, stimulus_type), axis=1) 
    
    return out

# GSR DDM simulator
# For a single trial

@njit 
def gsr_diffusion_trial(v, a, ndtcorrect, ndterror, sv, sndt):
    """Simulates a trial from the diffusion model."""

    n_steps = 0.
    x = a * 0.5
    dt = 0.001
    max_steps = 10000
    v = v + sv * np.random.normal()
    ndtcorrect = ndtcorrect - 0.5*sndt + sndt * np.random.uniform(0, 1)
    
    # Simulate a single DM path
    while (x > 0 and x < a and n_steps < max_steps):

        # DDM equation
        x += v*dt + np.sqrt(dt) * np.random.normal()

        # Increment step
        n_steps += 1.0

    rt = n_steps * dt
    return rt + ndtcorrect if x > 0. else -rt - ndterror # use different ndts for correct and error responses

# For an entire subject
@njit 
def gsr_ddm_simulator_fun(theta):
    """Simulates data from a single subject in an IAT experiment. Goes through experimental conditions in turn."""
    
    similarity, drift_word, drift_picture, threshold1, threshold2,  ndt_plus, ndt_minus, sv , sndt = theta
       

    num_trial_condition_1_word = 30
    num_trial_condition_1_picture = 30
    num_trial_condition_2_word = 30
    num_trial_condition_2_picture = 30

    
    total_trial_n = num_trial_condition_1_word +num_trial_condition_1_picture + num_trial_condition_2_word+ num_trial_condition_2_picture
    
    out_1_word = np.zeros(num_trial_condition_1_word)
    out_1_picture = np.zeros(num_trial_condition_1_picture)
    out_2_word = np.zeros(num_trial_condition_2_word)
    out_2_picture = np.zeros(num_trial_condition_2_picture)


 
    # Condition 1: Congruent
    drift_congruent_word = drift_word * (1+similarity)
    drift_congruent_picture = drift_picture * (1+similarity)
    
    for n in range(num_trial_condition_1_word):
        out_1_word[n] = gsr_diffusion_trial(drift_congruent_word, threshold1, ndt_plus, ndt_minus, sv, sndt)  
        
    for n in range(num_trial_condition_1_picture):
        out_1_picture[n] = gsr_diffusion_trial(drift_congruent_picture, threshold1, ndt_plus, ndt_minus, sv, sndt)  
        
    # Condition 2: Incongruent
    drift_incongruent_word = drift_word * (1-similarity)
    drift_incongruent_picture = drift_picture * (1-similarity)

    for n in range(num_trial_condition_2_word):
        out_2_word[n] = gsr_diffusion_trial(drift_incongruent_word, threshold2, ndt_plus, ndt_minus, sv, sndt)   
    
    for n in range(num_trial_condition_2_picture):
        out_2_picture[n] = gsr_diffusion_trial(drift_incongruent_picture, threshold2, ndt_plus, ndt_minus, sv, sndt)   
    
    out = np.concatenate((out_1_word, out_1_picture, out_2_word, out_2_picture))
    
    # mark too slow or too fast trials with zero
    for n in range(total_trial_n):
        if abs(out[n]) > 10.0:
            out[n] = 0
        if abs(out[n]) < 0.2:
            out[n] = 0
            
    obs_per_condition = 60        
    condition_type = np.arange(2)
    condition_type = np.repeat(condition_type, obs_per_condition)
    
    # code stimulus types, picture == 1
    stimulus_type = np.concatenate ((np.zeros(30), np.ones(30),# condition 1: congruent
                                       np.zeros(30), np.ones(30)) ) # condition 2: incongruent
    
    missings = np.expand_dims(np.zeros((out.shape[0])),1)
    missings [out==0] = 1
    
    out = np.expand_dims(out, 1)
    condition_type = np.expand_dims(condition_type, 1)
    stimulus_type = np.expand_dims(stimulus_type, 1)
    out = np.concatenate((out, missings, condition_type, stimulus_type), axis=1) 
    return out

def compute_summaries(samples_dm):
    """
    Compute summary statistics and correlations
    """          
    param_means = np.nanmean(samples_dm, axis=1)
    param_medians = np.nanmedian(samples_dm,axis=1)
    param_stds = np.nanstd(samples_dm,axis=1)
    param_q025 = np.nanquantile(samples_dm, .025, axis=1)  
    param_q975 = np.nanquantile(samples_dm, .975, axis=1)  
    
   
    estimates = np.concatenate((param_means, param_medians, param_stds,
                                param_q025, param_q975), axis=1) 
    return estimates
