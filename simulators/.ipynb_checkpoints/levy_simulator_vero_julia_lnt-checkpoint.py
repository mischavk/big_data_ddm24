from numba import jit, prange
from tensorflow.keras import utils
import ctypes
from numba.extending import get_cython_function_address
import numpy as np

# Get a pointer to the C function levy.c
addr_levy= get_cython_function_address("levy", "levy_trial")
functype = ctypes.CFUNCTYPE(ctypes.c_double, ctypes.c_double, ctypes.c_double, 
                            ctypes.c_double, ctypes.c_double, ctypes.c_double,
                            ctypes.c_double, ctypes.c_double, ctypes.c_double, 
                            ctypes.c_double, ctypes.c_double, ctypes.c_int)
levy_trial = functype(addr_levy)


@jit(nopython=True)
def levy_condition(v,  a, t0, st0, zr, n_obs, dt=0.001, max_steps=1e4):
    """A wrapper over the jit function."""

    x = np.empty((a.shape[0], n_obs))
    _levy_condition(v, a, t0, st0, zr, x, dt, max_steps)
    return x

@jit(nopython=True, parallel=True)
def _levy_condition(v,  a, t0, st0, zr, x, dt, max_steps):
    """
    Simulate a batch from the diffusion model.
    ----------
    INPUT:
    v      - np.array of shape (n_batch, )
    a      - np.array of shape (n_batch, )
    params - np.array of shape (n_batch, n_shared_params):
        param index 0 - t0
        param index 1 - alpha
    x      - np.array of shape (n_batch, n_obs) - zero padded arrays
    """

    # For each batch
    for i in prange(x.shape[0]):
        # For each trial
        for j in prange(x.shape[1]):
            x[i, j] = levy_trial(v[i], 0., zr[i], 0., 
                                a[i], t0[i], t0[i], st0[i], 
                                2.0, dt, max_steps)


def levy_simulator_2(params, n_obs=400, dt=0.001, max_steps=1e4):
    """
    Simulates a levy process for 1 condition with 7 parameters (t0, st0, alpha, zr,  v1, v2, a).
    """

    n1 =  n_obs // 2
    n2 = n_obs - n1
    t0 = params[:, 0]
    st0 = params[:, 1]
  #  alpha = params[:, 2]
    zr = params[:, 2]
    v1 = params[:, 3]
    v2 = params[:, 4]
    a = params[:, 5]


    
    rt_c1 = levy_condition(v1, a, t0, st0, zr, n1, dt=dt, max_steps=max_steps)
    rt_c2 = levy_condition(v2, a, t0, st0, zr, n2, dt=dt, max_steps=max_steps)

    # Store rts
    rts = np.concatenate([rt_c1, rt_c2], axis=1)[:,:,np.newaxis]

    # Create conditions array and one-hot-encode it
    ind = np.stack(params.shape[0] * [np.concatenate((np.zeros(n1), np.ones(n2)))]).astype(np.int32)
    oh = utils.to_categorical(ind)
    
    sim_data = np.concatenate((rts, oh), axis=-1)
    return sim_data
