
rule validate_covid:
    input:
        varsel_td_pol = "../results/models/varsel_td_pol.rds",
        covid_data = "../results/processed_data/covid_dat.rds",
        fit_ref = "../results/models/td_ref_pol_fit.rds"
    output:
        plot_1 = "../results/models/td_plot_yhat_cv_y.pdf",
    log:
        "../results/logs/validate_covid.log"
    script:
        "../scripts/analysis/04_td_validate_covid.R"
