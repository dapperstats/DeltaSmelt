# Juvenile delta smelt distribution model
# JL Simonis and JE Merz
#
# Models used
#
# Model      Description
# 1          Poisson count drawn from global intercept
# 2          Poisson count drawn from visit-level density drawn from global
#              intercept
# 3          Poisson count drawn from visit-level density drawn from 
#              regression
# 4          Poisson count drawn from visit-level density drawn from 
#              regression scaled to tow-level density by velocity via a 
#              forced negative exponential
# 5          Poisson count drawn from visit-level density drawn from 
#              regression scaled to tow-level density by velocity via an 
#              exponential scaling term drawn from a regression 
# 6          Poisson count drawn from visit-level density drawn from 
#              spatially autocorrelated regression scaled to tow-level 
#              density by velocity via an exponential scaling term 
#              drawn from a regression 
# 7          Poisson count drawn from visit-level density drawn from 
#              temporally autocorrelated regression scaled to tow-level 
#              density by velocity via an exponential scaling term 
#              drawn from a regression 
# 8          Poisson count drawn from visit-level density drawn from 
#              spatially and temporally autocorrelated regression scaled to 
#              tow-level density by velocity via an exponential scaling term 
#              drawn from a regression 
# 9          Poisson count drawn from visit-level density drawn from 
#              spatiotemporally autocorrelated regression scaled to 
#              tow-level density by velocity via an exponential scaling term 
#              drawn from a regression 


#
# Model      Parameters
# 1          beta[1]
# 2          beta[1], tau_ldelta
# 3          beta[1:13], tau_ldelta
# 4          beta[1:13], tau_ldelta, chi
# 5          beta[1:13], tau_ldelta, chi, tau_lzeta
# 6          beta[1:13], tau_ldelta, chi, tau_lzeta, phi, tau_leta
# 7          beta[1:13], tau_ldelta, chi, tau_lzeta, lambda, tau_lgamma
# 8          beta[1:13], tau_ldelta, chi, tau_lzeta, phi, tau_leta, 
#              lambda, tau_lgamma
# 9          beta[1:13], tau_ldelta, chi, tau_lzeta, phi,  
#              lambda, tau_lkappa
