
# ===============================
# ==== MAIN USABILITY SCRIPT ====
# ===============================

# Esta package permite processamento paralelo de operacoes,
# o que facilita e acelera calculos necessarios
# install.packages('doMC')
library('doMC')
registerDoMC(cores=4)

# Leitura do ficheiro vpn_sessions.txt

