
rule varsel_pol_model:
    input:
        fitted_model = "../results/models/td_ref_pol_fit.rds",
        pol_data = "../results/processed_data/pol_dat.rds"
    output:
        varsel_td_pol = "../results/models/varsel_td_pol.rds"
    log:
        "../results/logs/varsel_pol_model.log"
    script:
        "../scripts/analysis/02_td_varsel_ref_pol_model.R"


rule get_subset_pol_model:
    input:
        varsel_td_pol = "../results/models/varsel_td_pol.rds",
        pol_data = "../results/processed_data/pol_dat.rds",
        fit_ref = "../results/models/td_ref_pol_fit.rds"
    output:
        plot_1 = "../results/models/td_plot_yhat_sub_tot.pdf",
        plot_2 = "../results/models/td_yhat_tot_y.pdf",
        plot_3 = "../results/models/td_yhat_subset_y.pdf"
    log:
        "../results/logs/get_subset_pol_model.log"
    script:
        "../scripts/analysis/03_td_get_sug_size_pol.R"
