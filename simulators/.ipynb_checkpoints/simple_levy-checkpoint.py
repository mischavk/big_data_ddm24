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
def levy_condition(t0, v, zr, a, alpha, sv, szr, st0, n_obs, dt=0.001, max_steps=1e4):
    """A wrapper over the jit function."""

    x = np.empty((v.shape[0], n_obs))
    _levy_condition(t0, v, zr, a, alpha, sv, szr, st0, x, dt, max_steps)
    return x

@jit(nopython=True, parallel=True)
def _levy_condition(t0, v, zr, a, alpha, sv, szr, st0, x, dt, max_steps):
    """
    Simulate a batch from the diffusion model.
    ----------
    INPUT:
    v      - np.array of shape (n_batch, )
    alpha  - np.array of shape (n_batch, )
    ...

    x      - np.array of shape (n_batch, n_obs) - zero padded arrays
    """

    # For each batch
    for i in prange(x.shape[0]):
        # For each trial
        for j in prange(x.shape[1]):
            x[i, j] = levy_trial(v[i], 
                                 sv, 
                                 zr, 
                                 szr, 
                                 a[i], 
                                 t0[i], 
                                 t0[i],
                                 st0, 
                                 alpha[i],
                                 dt, max_steps)


def levy_simulator(params, n_obs, dt=0.001, max_steps=1e4):
    """
    Simulates a levy flight process for 1 condition with 4 parameters (t0, v, a, alpha).
    """

    n1 =  n_obs 

    t0 = params[:, 0]
    st0 = 0
    zr =0.5
    szr=0
    v = params[:, 1]
    a = params[:, 2]
    sv = 0
    szr = 0
    alpha = params[:, 3]
    
    rt_c1 = levy_condition(t0, v, zr, a, alpha, sv, szr, st0, n1, dt=dt, max_steps=max_steps)

    # Store rts
    rts = np.concatenate([rt_c1], axis=1)[:,:,np.newaxis]

    return rts
