# Install/load necessary packages
  library(baseballr)
  library(dplyr)
  library(lubridate)
  library(janitor)
  library(ggplot2)
  library(devtools)
  library(tidyr)
  library(patchwork)
  library(rvest)
  devtools::install_github("BillPetti/baseballr")

  
# Compile and format Trevor Story's data for analysis
  
  
  ## Retrieve statcast data
      storyID <- 596115
      roughstoryData <- statcast_search(
        start_date = "2025-03-27",
        end_date = "2025-07-15",
        playerid = storyID,
        player_type = "batter"
      )
      
  ## Split data up into pre-/post-June 1st
      storyData <- roughstoryData %>%
        mutate(period = if_else(game_date < as.Date("2025-06-01"), "Before", "After"))
      
      
  ## First, look at standard batting stats (AVG, SLG, OPS, OPS+, etc.)
      ### Count hits, total bases, at bats, etc. to calculate statistics after filtering non-outcome pitches
        storyBatting <- storyData %>%
          filter(!is.na(events)) %>%
          mutate(
            hit = events %in% c("single", "double", "triple", "home_run"),
            bases = case_when(
              events == "single" ~ 1,
              events == "double" ~ 2,
              events == "triple" ~ 3,
              events == "home_run" ~ 4,
              .default = 0
            ),
            atbat = events %in% c("single", "double", "home_run", "strikeout", "field_out",
                                  "force_out", "double_play", "field_error", "grounded_into_double_play"),
            plateapp = events %in% c("single", "double", "home_run", "strikeout", "field_out",
                                     "force_out", "double_play", "field_error", "grounded_into_double_play", "walk",
                                     "hit_by_pitch", "sac_bunt"),
            walk = events == "walk",
            homerun = events == "home_run",
            k = events == "strikeout"
          ) %>% 
          group_by(period) %>%
          summarize(
            H = sum(hit, na.rm = TRUE),
            TB = sum(bases, na.rm = TRUE),
            AB = sum(atbat, na.rm = TRUE),
            PA = sum(plateapp, na.rm = TRUE),
            BB = sum(walk, na.rm = TRUE),
            HR = sum(homerun, na.rm = TRUE),
            K = sum(k, na.rm = TRUE),
            SF = 0,
            AVG = H/AB,
            SLG = TB/AB,
            OBP = (H+BB)/(AB+BB), # Using simple formula, does not include HBP and sac
            OPS = OBP+SLG,
            BABIP = (H-HR)/(AB-HR-K+SF),
            Kpct = K/(AB+BB),
            BBpct = BB/(AB+BB),
          )
      
      ### Calculate OPS+ (simplified, not adjusted for park effects)
        leaguestats <- fg_bat_leaders(
          startseason = 2025,
          endseason = 2025,
          stats = "bat",
          ind = 1,
          qual = 1,   #### Stats only for batters with at least 1 plate appearance
          pos = "np"
        )             
        leagueOBP = mean(leaguestats$OBP, na.rm = TRUE)
        leagueSLG = mean(leaguestats$SLG, na.rm = TRUE)
        storyBatting <- storyBatting %>%
          mutate(OPSplus = round(100*(((storyBatting$OBP/leagueOBP)+(storyBatting$SLG/leagueSLG))-1)))
  
  ## Obtain statcast metrics
    storyStatcast <- storyData %>% 
      filter(!is.na(launch_speed)) %>%
      mutate(
        hardhit = launch_speed >= 95,
        battedball = events %in% c("single", "double", "triple", "home_run", "groundout",
                                   "flyout", "field_out","force_out", "double_play", 
                                   "field_error", "grounded_into_double_play")
      ) %>%
      group_by(period) %>%
      summarize(
        avgEV = mean(launch_speed, na.rm = TRUE),
        avgLA = mean(launch_angle, na.rm = TRUE),
        battedballsnum = sum(battedball, na.rm = TRUE),
        hardhitnum = sum(hardhit & battedball, na.rm = TRUE),
        hardhitrate = hardhitnum/battedballsnum
      )
  
  ## Compile all data into one set
    storyTotals <- storyBatting %>%
      left_join(storyStatcast, by = "period") %>%
      arrange(desc(period))
    
    
    
  ## Create bar plots to visualize differences in data
    storysmalltotals <- storyTotals %>%
      select(period, AVG, SLG, OPS, BABIP, hardhitrate, Kpct, BBpct) %>%
      pivot_longer(-period, names_to = "Metric", values_to = "Value") %>%
      mutate(period = factor(period, levels = c("Before", "After"))) %>%
      ggplot(aes(x = Metric, y = Value, fill = period)) +
      ggtitle("Proportion-Based Metrics Before and After June 1") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_col(position = "dodge") +
      scale_y_continuous(limits = c(0,1)) 
    storybigtotals <- storyTotals %>%
      select(period, OPSplus, avgEV, avgLA) %>%
      pivot_longer(-period, names_to = "Metric", values_to = "Value") %>%
      mutate(period = factor(period, levels = c("Before", "After"))) %>%
      ggplot(aes(x = Metric, y = Value, fill = period)) +
      ggtitle("Batted Ball Metrics and OPS+ Before and After June 1") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_col(position = "dodge") +
      scale_y_continuous(limits = c(0,185))
    storysmalltotals
    storybigtotals
  
    
    
# Analyze compiled data using relevant statistical tests and analytical methods to answer research questions
    
    storyGamestats <- storyData %>%    ### Compiles data and groups by game to be able to perform t-tests
      filter(!is.na(events)) %>%
      mutate(
        hit = events %in% c("single", "double", "triple", "home_run"),
        bases = case_when(
          events == "single" ~ 1,
          events == "double" ~ 2,
          events == "triple" ~ 3,
          events == "home_run" ~ 4,
          .default = 0
        ),
        atbat = events %in% c("single", "double", "triple", "home_run", "groundout", "strikeout", "flyout", "field_out",
                              "force_out", "double_play", "field_error", "grounded_into_double_play"),
        walk = events == "walk",
        homerun = events == "home_run",
        k = events == "strikeout"
      ) %>% 
      group_by(game_date, period) %>%
      summarize(
        avgEV = mean(launch_speed, na.rm = TRUE),
        avgLA = mean(launch_angle, na.rm = TRUE),
        H = sum(hit, na.rm = TRUE),
        TB = sum(bases, na.rm = TRUE),
        AB = sum(atbat, na.rm = TRUE),
        BB = sum(walk, na.rm = TRUE),
        HR = sum(homerun, na.rm = TRUE),
        K = sum(k, na.rm = TRUE),
        SF = 0,
        AVG = H/AB,
        SLG = TB/AB,
        OBP = (H+BB)/(AB+BB), # Using simple formula, does not include HBP and sac
        OPS = OBP+SLG,
        BABIP = (H-HR)/(AB-HR-K+SF),
        Kpct = K/(AB+BB),
        BBpct = BB/(AB+BB),
        .groups = "drop"
      ) %>%
      filter(!is.na(OPS))
    
  
  ## Question: Is Story's improvement since June 1st real and significant?
    
    ### Perform different tests and collect results into data frame
      propResults <- data.frame(
        Metric = c("AVG", "SLG", "OPS", "BABIP", "EV", "LA", "HardHit%", "K%", "BB%"),
        P_Value = c(
          prop.test(storyBatting$H, storyBatting$AB, alternative = "two.sided")$p.value,
          wilcox.test(SLG ~ period, data = storyGamestats)$p.value,
          wilcox.test(OPS ~ period, data = storyGamestats)$p.value,
          prop.test((storyBatting$H-storyBatting$HR), (storyBatting$AB-storyBatting$K-storyBatting$HR))$p.value,
          t.test(launch_speed ~ period, data = storyData)$p.value,
          t.test(launch_angle ~ period, data = storyData)$p.value,
          prop.test(storyTotals$hardhitnum, storyTotals$battedballsnum)$p.value,
          prop.test(storyTotals$K, storyTotals$PA)$p.value,
          prop.test(storyTotals$BB, storyTotals$PA)$p.value
  
        )
      )

      
      
      
      
  ## Question: Is he doing more damage vs certain pitch types now vs before?
      
    ### Extract data against different pitch types
      storyPitchStats <- storyData %>%
        mutate(
          pitch_group = case_when(
            pitch_name == "4-Seam Fastball" ~ "4-Seam Fastball",
            pitch_name %in% c("Curveball", "Knuckle Curve") ~ "Curveball",
            pitch_name %in% c("Changeup", "Split-Finger") ~ "Offspeed",
            pitch_name == "Sinker" ~ "Sinker",
            pitch_name == "Cutter" ~ "Cutter",
            pitch_name == "Slider" ~ "Slider",
            pitch_name == "Sweeper" ~ "Sweeper",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(pitch_group), !is.na(pitch_name), !is.na(events)) %>%
        mutate(
          hit = events %in% c("single", "double", "triple", "home_run"),
          bases = case_when(
            events == "single" ~ 1,
            events == "double" ~ 2,
            events == "triple" ~ 3,
            events == "home_run" ~ 4,
            .default = 0
          ),
          atbat = events %in% c("single", "double", "home_run", "strikeout", "field_out",
                                "force_out", "double_play", "field_error", "grounded_into_double_play"),
          plateapp = events %in% c("single", "double", "home_run", "strikeout", "field_out",
                                   "force_out", "double_play", "field_error", "grounded_into_double_play", "walk",
                                   "hit_by_pitch", "sac_bunt"),
          walk = events == "walk",
          homerun = events == "home_run",
          k = events == "strikeout"
        ) %>%
        group_by(period, pitch_group) %>%
        summarize(
          Pitches = n(),
          avgEV = mean(launch_speed, na.rm = TRUE),
          avgLA = mean(launch_angle, na.rm = TRUE),
          hardhitrate = mean(launch_speed >= 95, na.rm = TRUE),
          H = sum(hit, na.rm = TRUE),
          TB = sum(bases, na.rm = TRUE),
          AB = sum(atbat, na.rm = TRUE),
          BB = sum(walk, na.rm = TRUE),
          HR = sum(homerun, na.rm = TRUE),
          K = sum(k, na.rm = TRUE),
          SF = 0,
          AVG = H / AB,
          SLG = TB / AB,
          OBP = (H + BB) / (AB + BB),  # Approximate OBP (not including HBP or sac flies)
          OPS = OBP + SLG,
          BABIP = (H - HR) / (AB - HR - K + SF),
          Kpct = K / (AB + BB),
          BBpct = BB / (AB + BB),
          .groups = "drop"
        )
      
        

      
    ### Create bar plots to visualize performance vs. different pitch types
      
      #### Plot for AVG
      storyPitchAvgplot <- storyPitchStats %>%
        mutate(period = factor(period, levels = c("Before", "After"))) %>%
        ggplot(aes(x = pitch_group, y = AVG, fill = period)) +
        geom_col(position = "dodge") +
        ggtitle("AVG by Pitch Type Before and After June 1") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "Pitch Type", y = "Batting Average") +
        scale_y_continuous(limits = c(0, .6))
    storyPitchAvgplot
    
    #### Plot for SLG
    storyPitchSlgplot <- storyPitchStats %>%
      mutate(period = factor(period, levels = c("Before", "After"))) %>%
      ggplot(aes(x = pitch_group, y = SLG, fill = period)) +
      geom_col(position = "dodge") +
      ggtitle("SLG by Pitch Type Before and After June 1") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Pitch Type", y = "Slugging Percentage") +
      scale_y_continuous(limits = c(0, 1.4))
    storyPitchSlgplot
    
    #### Plot for HardHit%
    storyPitchHHplot <- storyPitchStats %>%
      mutate(period = factor(period, levels = c("Before", "After"))) %>%
      ggplot(aes(x = pitch_group, y = hardhitrate, fill = period)) +
      geom_col(position = "dodge") +
      ggtitle("Hard Hit Rate by Pitch Type Before and After June 1") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Pitch Type", y = "Hard Hit Rate") +
      scale_y_continuous(limits = c(0, 0.5))
    storyPitchHHplot

    