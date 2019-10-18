expected_cols <- cols(
  directory = col_character(),
  experiment = col_character(),
  ppid = col_character(),
  session_num = col_double(),
  trial_num = col_double(),
  block_num = col_double(),
  trial_num_in_block = col_double(),
  start_time = col_double(),
  end_time = col_double(),
  selected_chest = col_character(),
  select_chest_time = col_double(),
  selected_cup = col_character(),
  select_cup_time = col_double(),
  outcome = col_character()
)

# read all behavioural data, and fix any issues with it
trials_df <- list.files(
  path = "data",
  pattern = "trial_results.csv",
  recursive = TRUE, 
  full.names = TRUE
  ) %>% 
  map_dfr(read_csv, col_types = expected_cols) %>% 
  separate(outcome, into = c("stage1", "stage2rev", "stage2outcome"), remove = FALSE) %>% 
  unite("stage2", c("stage2rev", "stage2outcome")) %>% 
  group_by(ppid) %>% 
  mutate(reselect = selected_chest == lag(selected_chest),
         chest_1_probe = chest_1_probe / 100,
         chest_2_probe = chest_2_probe / 100,
         outcome = recode_factor(outcome,
                                 correct_open_reward = "Reward",
                                 correct_open_empty = "Chest error",
                                 incorrect_closed_unknown = "Cup error"),
         selected_chest = recode_factor(selected_chest,
                                        `Stage2Error+` = "CupError+",
                                        `Stage1Error+` = "ChestError+"),
         prev_chest = lag(selected_chest),
         prev_outcome = lag(outcome))

pplist <- read_csv("data/participant_list.csv",
                   col_types = cols(
                     ppid = col_character(),
                     flipped = col_logical(),
                     age = col_double(),
                     user = col_character()
                   ))

select_df <- trials_df %>% 
  group_by(ppid, selected_chest) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  select(-n) %>% 
  spread(selected_chest, freq) %>% 
  mutate(selection_bias = `CupError+` - `ChestError+`)

reselect_df <- trials_df %>% 
  filter(trial_num != 1) %>% 
  group_by(ppid, prev_outcome, prev_chest) %>% 
  summarise(n = n(), reselect_n = sum(reselect), reselect_freq = reselect_n / n) %>% 
  group_by(ppid) %>% 
  mutate(reselect_overall = sum(reselect_n) / sum(n),
         reselect_controlled = reselect_freq - reselect_overall)

value_df <- trials_df %>% 
  filter(!is.na(chest_1_probe)) %>% 
  left_join(pplist, by = "ppid") %>% 
  mutate(`CupError+` = ifelse(flipped, chest_2_probe, chest_1_probe),
         `ChestError+` = ifelse(flipped, chest_1_probe, chest_2_probe)) %>% 
  gather(chest, value_estimate, `CupError+`:`ChestError+`) %>% 
  mutate(chest = factor(chest, levels = c("CupError+", "ChestError+"))) %>% 
  ungroup()

value_summary_df <- value_df %>%
  group_by(ppid, chest) %>% 
  summarise(mean_value_estimate = mean(value_estimate))

value_select_summary_df <- value_summary_df %>% 
  spread(chest, mean_value_estimate) %>% 
  mutate(estimate_bias = `CupError+` - `ChestError+`) %>% 
  select(ppid, estimate_bias) %>% 
  left_join(select_df, by = "ppid")


# blocks of 30 trials (exploratory analysis)

b30_value_select_summary_df <- trials_df %>% 
  mutate(block_30 = 1 + (trial_num - 16) %/% 30) %>% 
  filter(block_30 > 0, block_30 <= 9) %>% 
  group_by(ppid, block_30) %>% 
  summarise(`CupError+` = mean(selected_chest == "CupError+"),
            `ChestError+` = mean(selected_chest == "ChestError+")) %>% 
  mutate(selection_bias = `CupError+` - `ChestError+`) %>% 
  ungroup() %>% 
  left_join(value_df %>%
              spread(chest, value_estimate) %>% 
              transmute(
                ppid,
                estimate_bias = `CupError+` - `ChestError+`,
                block_30 = trial_num / 30
                ),
                by = c("ppid", "block_30")
            )
