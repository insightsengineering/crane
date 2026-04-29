# df_add_poolings() validates inputs and throws correct cli errors

    Code
      df_add_poolings(adam_db = data.frame(), pools = standard_pools)
    Condition
      Error in `df_add_poolings()`:
      x The arm variable "TRT01A" was not found in any dataset within `adam_db`.
      i Available columns in `adsl`: .

---

    Code
      df_add_poolings(adam_db = list(adsl = adsl_dummy, bad = "string"), pools = standard_pools)
    Condition
      Error in `df_add_poolings()`:
      x `adam_db` must be a list of data frames.
      i You provided an object of class <list>.

---

    Code
      df_add_poolings(adam_db = list(adxx = adxx_dummy), pools = standard_pools,
      arm_var = "TRT01A")
    Condition
      Error in `df_add_poolings()`:
      x The arm variable "TRT01A" was not found in any dataset within `adam_db`.
      i Available columns in `adsl`: .

---

    Code
      df_add_poolings(adam_db = adam_db_test, pools = c("Drug A", "Drug B"))
    Condition
      Error in `df_add_poolings()`:
      x `pools` must be a fully named list.
      i Ensure every element in your list has a label (e.g., `list("Group A" = c(...))`).

---

    Code
      df_add_poolings(adam_db = adam_db_test, pools = list(`Drug A + B` = c("Drug A",
        "Drug B"), c("Drug C")))
    Condition
      Error in `df_add_poolings()`:
      x `pools` must be a fully named list.
      i Ensure every element in your list has a label (e.g., `list("Group A" = c(...))`).

