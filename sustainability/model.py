#!/usr/bin/env python3

import numpy as np
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Agg')

# ── Parameters ────────────────────────────────────────────────────────────────

Atotal = 100.0
Rtotal = 10.0

p = dict(
    am   = 0.0001,
    aa   = 0.00011,
    awv  = 0.05,
    aw   = 0.001,
    ai   = 0.001,

    krm  = 0.001,
    krw  = 0.001,
    kwm  = 0.05,   # was 0,05 (comma = bug) in parameters.r
    kxn  = 0.005,

    betavv = 0.008,  betaav = 0.008,  betaiv = 0.008,
    betava = 0.008,  betaaa = 0.008,  betaia = 0.008,
    betavi = 0.008,  betaai = 0.008,  betaii = 0.008,

    mad  = 0.05,
    mjuv = 0.06,
    ra   = 0.5,
    rv   = 1.0,
    ri   = 0.5,
    rx   = 2.0,
    Kp   = 100.0,
    Kc   = 500.0,
    kmc  = 0.01,
)

# ── Initial state ─────────────────────────────────────────────────────────────
#  Order: Av, Aa, Ai, Aw, Rv, Rm, Rw, x, nv, yv, na, ya, ni, yi

state0 = [
    Atotal - 3,   # Av
    1.0,          # Aa
    1.0,          # Ai
    1.0,          # Aw
    Rtotal,       # Rv  (implicit: Rtotal - Rm - Rw)
    0.0,          # Rm
    0.0,          # Rw
    10.0,         # x
    50.0,         # nv
    50.0,         # yv
    50.0,         # na
    0.5,          # ya
    5.0,          # ni
    0.5,          # yi
]

# ── ODE system ────────────────────────────────────────────────────────────────

def worldmodel(t, s, p):
    Av, Aa, Ai, Aw, Rv, Rm, Rw, x, nv, yv, na, ya, ni, yi = s

    am=p['am']; aa=p['aa']; awv=p['awv']; aw=p['aw']; ai=p['ai']
    krm=p['krm']; krw=p['krw']; kwm=p['kwm']; kxn=p['kxn']
    betavv=p['betavv']; betaav=p['betaav']; betaiv=p['betaiv']
    betava=p['betava']; betaaa=p['betaaa']; betaia=p['betaia']
    betavi=p['betavi']; betaai=p['betaai']; betaii=p['betaii']
    mad=p['mad']; mjuv=p['mjuv']
    ra=p['ra']; rv=p['rv']; ri=p['ri']; rx=p['rx']
    Kp=p['Kp']; Kc=p['Kc']; kmc=p['kmc']

    Rv_eff = Rtotal - Rm - Rw   # conserved: don't use the Rv state variable

    # Areas
    dAv = -(am*ni + aa*na)*Av + awv*Aw
    dAa =  aa*na*Av - aw*Aa
    dAi =  am*ni*Av - ai*Ai
    dAw =  aw*Aa + ai*Ai - awv*Aw

    # Resources
    dRv = -krm * Rv_eff * ni
    dRm =  krm * Rv_eff * ni - krw*Rm + kwm*ni*Rw
    dRw =  krw*Rm - kwm*ni*Rw

    # Hunter-gatherer prey
    dx  = rx*x*(1 - x/(Av*Kp)) - kxn*x*nv

    # Hunter-gatherer population
    dnv = (betavv*yv + betaav*ya + betaiv*yi - mad)*nv
    dyv =  rv*kxn*x*nv - (betavv*nv + betava*na + betavi*ni + mjuv)*yv

    # Farming population
    dna = (betava*yv + betaaa*ya + betaia*yi - mad)*na
    dya =  ra*na*(1 - (na+ni)/(Aa*(Kc + kmc*Rm))) \
           - (betaav*nv + betaaa*na + betaai*ni + mjuv)*ya

    # Industrial population
    dni = (betavi*yv + betaai*ya + betaii*yi - mad)*ni
    dyi =  ri*ni*(1 - (na+ni)/(Aa*(Kc + kmc*Rm))) \
           - (betaiv*nv + betaia*na + betaii*ni + mjuv)*yi

    return [dAv, dAa, dAi, dAw, dRv, dRm, dRw,
            dx, dnv, dyv, dna, dya, dni, dyi]

# ── Solve ─────────────────────────────────────────────────────────────────────

t_span = (0, 500)
t_eval = np.arange(0, 501, 1.0)

sol = solve_ivp(
    worldmodel, t_span, state0,
    args=(p,),
    method='LSODA',
    t_eval=t_eval,
    rtol=1e-6, atol=1e-9,
    dense_output=False
)

if not sol.success:
    print("Solver failed:", sol.message)
    exit(1)

t  = sol.t
Av, Aa, Ai, Aw = sol.y[0], sol.y[1], sol.y[2], sol.y[3]
Rv, Rm, Rw     = sol.y[4], sol.y[5], sol.y[6]
x              = sol.y[7]
nv, yv         = sol.y[8], sol.y[9]
na, ya         = sol.y[10], sol.y[11]
ni, yi         = sol.y[12], sol.y[13]

# ── Plot ──────────────────────────────────────────────────────────────────────

fig, axes = plt.subplots(3, 2, figsize=(12, 10))
fig.suptitle("World Model — baseline run", fontsize=13)

# Land areas
ax = axes[0, 0]
ax.plot(t, Av, label='$A_v$ virgin')
ax.plot(t, Aa, label='$A_a$ agricultural')
ax.plot(t, Ai, label='$A_i$ industrial')
ax.plot(t, Aw, label='$A_w$ waste')
ax.set_title('Land areas')
ax.set_xlabel('Time')
ax.legend(fontsize=8)

# Resources
ax = axes[0, 1]
ax.plot(t, Rtotal - Rm - Rw, label='$R_v$ virgin')
ax.plot(t, Rm, label='$R_m$ in use')
ax.plot(t, Rw, label='$R_w$ waste')
ax.set_title('Resources')
ax.set_xlabel('Time')
ax.legend(fontsize=8)

# Hunter-gatherer
ax = axes[1, 0]
ax.plot(t, x,  label='$x$ prey')
ax.plot(t, nv, label='$n_v$ adults')
ax.plot(t, yv, label='$y_v$ youth')
ax.set_title('Hunter-gatherer subsystem')
ax.set_xlabel('Time')
ax.legend(fontsize=8)

# Farming
ax = axes[1, 1]
ax.plot(t, na, label='$n_a$ adults')
ax.plot(t, ya, label='$y_a$ youth')
ax.set_title('Farming subsystem')
ax.set_xlabel('Time')
ax.legend(fontsize=8)

# Industrial
ax = axes[2, 0]
ax.plot(t, ni, label='$n_i$ adults')
ax.plot(t, yi, label='$y_i$ youth')
ax.set_title('Industrial subsystem')
ax.set_xlabel('Time')
ax.legend(fontsize=8)

# Population overview
ax = axes[2, 1]
ax.plot(t, nv, label='$n_v$ hunter-gatherer')
ax.plot(t, na, label='$n_a$ farming')
ax.plot(t, ni, label='$n_i$ industrial')
ax.set_title('Adult populations — all three')
ax.set_xlabel('Time')
ax.legend(fontsize=8)

plt.tight_layout()
plt.savefig('/mnt/user-data/outputs/baseline.png', dpi=150)
print("Done. Solver message:", sol.message)
print(f"Steps taken: {len(t)}")
print(f"\nFinal state at t=500:")
print(f"  Av={Av[-1]:.3f}  Aa={Aa[-1]:.3f}  Ai={Ai[-1]:.3f}  Aw={Aw[-1]:.3f}")
print(f"  Rv={Rtotal-Rm[-1]-Rw[-1]:.3f}  Rm={Rm[-1]:.3f}  Rw={Rw[-1]:.3f}")
print(f"  nv={nv[-1]:.3f}  na={na[-1]:.3f}  ni={ni[-1]:.3f}")
