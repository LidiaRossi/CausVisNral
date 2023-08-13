library(tidyverse)
library(reticulate)

if(!dir.exists('data/')){dir.create('data/'); cat('Data folder created.') }

library(tidyverse)
library(reticulate)

#### helper functions ####

# get stimuli table given experiment sdk address
get_stimuli_table <- function(SDK_ADDRESS){
    out <- map_dfr(SDK_ADDRESS$list_stimuli(),
                   ~ as_tibble(SDK_ADDRESS$get_stimulus_table(.x)) %>%
                       mutate(stimulus = .x) %>%
                       select(stimulus, start, end)
    )

    return(out)
}


get_block_structure2 <- function(SDK_ADDRESS){

    # get stimuli table via api
    stim_tab <- get_stimuli_table(SDK_ADDRESS) %>% arrange(start)

    # identify stimuli change points
    idx <- which(diff(as.numeric(factor(stim_tab$stimulus, ordered = T)))!=0)
    tmp_tab <- tibble(
        i = idx,
        before = stim_tab$stimulus[idx],
        after = stim_tab$stimulus[idx + 1],
        before_end = stim_tab$end[idx],
        after_start = stim_tab$start[idx+1])

    # initialise complete stimuli tab
    full_tab <-  tibble(stimulus = 'none',
                        start = 1,
                        end = as.numeric(stim_tab[1,'start']-1)) %>%
        bind_rows(tibble(stimulus = as.character(tmp_tab[1,'before']),
                         start = as.numeric(stim_tab[1,'start']),
                         end = as.numeric(tmp_tab[1, 'before_end']) ))

    # loop to construct full tab
    for (i in 2:nrow(tmp_tab)) {



        if((as.numeric(tmp_tab[i-1,'after_start' ]) - as.numeric(tmp_tab[i-1, 'before_end']))>1){
            gap_row <-  tibble(stimulus = 'none',
                               start = as.numeric(tmp_tab[i-1,'before_end'] + 1),
                               end = as.numeric(tmp_tab[i-1, 'after_start'] - 1))
            full_tab <- full_tab %>%
                bind_rows(gap_row)
        }
        new_row <- tibble(stimulus = as.character(tmp_tab[i,'before']),
                          start = as.numeric(full_tab[nrow(full_tab),'end'] + 1),
                          end = as.numeric(tmp_tab[i,'before_end'] ) )

        full_tab <- full_tab %>%
            bind_rows(new_row)

    }

    # finalise output
    # out <- full_tab %>%
    #     bind_rows(tibble(stimulus = 'none',
    #                      start = as.numeric(full_tab[nrow(full_tab),'end'] + 1),
    #                      end = length(SDK_ADDRESS$get_dff_traces()[[1]]) )
    #     )%>%
    #     mutate(block = row_number(), len = end - start + 1) %>% select(block, stimulus, start, end, len)

    # finalise output
    out <- full_tab %>%
        bind_rows(tibble(stimulus = 'none',
                         start = as.numeric(full_tab[nrow(full_tab),'end'] + 1),
                         end = as.numeric(tmp_tab[nrow(tmp_tab), 'after_start']))
        )%>%
        bind_rows(tibble(stimulus = as.character(tmp_tab[nrow(tmp_tab), 'after']),
                         start = as.numeric(tmp_tab[nrow(tmp_tab), 'after_start'] )+ 1,
                         end = as.numeric(stim_tab[nrow(stim_tab), 'end']))
        ) %>%
        bind_rows(tibble(stimulus = 'none',
                         start = as.numeric(stim_tab[nrow(stim_tab), 'end'])+1,
                         end = length(SDK_ADDRESS$get_dff_traces()[[1]]))
        ) %>%
        mutate(block = row_number(), len = end - start + 1) %>% select(block, stimulus, start, end, len)


    return(out)
}

get_stimuli_vector2 <- function(BLOCK_STRUCTURE2){

    out <- c()
    for (i in 1:nrow(BLOCK_STRUCTURE2)) {
        start <- as.numeric(BLOCK_STRUCTURE2[i,'start'])
        end <- as.numeric(BLOCK_STRUCTURE2[i,'end'])
        stimulus <- BLOCK_STRUCTURE2[i,'stimulus']
        out[start:end] <- stimulus
    }

    return(unlist(out))
}
get_block_vector2 <- function(BLOCK_STRUCTURE2){

    out <- c()
    for (i in 1:nrow(BLOCK_STRUCTURE2)) {
        start <- as.numeric(BLOCK_STRUCTURE2[i,'start'])
        end <- as.numeric(BLOCK_STRUCTURE2[i,'end'])
        block <- BLOCK_STRUCTURE2[i,'block']
        out[start:end] <- block
    }

    return(unlist(out))
}

get_spikes2 <- function(SDK_ADDRESS, ID){
    blocks <- get_block_structure2(SDK_ADDRESS)
    # stim_vec <- get_stimuli_vector2(blocks)
    # blocks_vec <- get_block_vector2(blocks)
    cells_labs <- paste0('cell_', SDK_ADDRESS$get_cell_specimen_ids())

    out <- as_tibble(t(boc$get_ophys_experiment_events(ID)))
    colnames(out) <- cells_labs

    out <- out %>%
        mutate(stimulus = get_stimuli_vector2(blocks),
               frame = row_number(),
               block = get_block_vector2(blocks)) %>%
        select(frame, block, stimulus, everything())

    return(out)
}

#### Import data ####
# pip install allensdk
BrainObservatoryCache <- import("allensdk.core.brain_observatory_cache", convert = TRUE)$BrainObservatoryCache
boc <- BrainObservatoryCache(manifest_file = 'boc/manifest.json')

chosen_depths <- c(375, 175)
chosen_sessions <- c('three_session_B', 'three_session_A')
chosen_structures <- c('VISp')

experiments_table <- map_dfr(
    boc$get_ophys_experiments(),
    ~as_tibble(t(unlist(.x))) ) %>%
    filter(imaging_depth %in% chosen_depths,
           session_type %in% chosen_sessions,
           targeted_structure %in% chosen_structures,
           fail_eye_tracking==F) %>%
    select(id, imaging_depth, targeted_structure, session_type, donor_name) #%>%

experiments_table <- experiments_table %>%
    mutate(
        id = as.numeric(id),
        sdk_address = map(id, ~try(boc$get_ophys_experiment_data(.x))),
        stimuli_structure = map(sdk_address, ~try(get_block_structure2(.x))),
        tidy_spikes = map2(id, sdk_address, ~try(get_spikes2(.y, .x)))
    )

saveRDS(experiments_table, file = 'data/spikes_traces.rds')

#### Compute outcomes ####

# Be careful: check for downloading errors! In the following code,
# corrupted experiments are automatically discarded
outcome_table <- experiments_table %>%
    mutate(
        spikes_count = map(tidy_spikes,
                           ~try(.x %>%
                                    group_by(block, stimulus) %>%
                                    summarise_at(vars(starts_with('cell')),
                                                 function(x) sum(x>0)))),
        spikes_average_count = map(tidy_spikes,
                                   ~try(.x %>%
                                            group_by(block, stimulus) %>%
                                            summarise_at(vars(starts_with('cell')),
                                                         function(x) mean(x>0)))),
        spikes_average_intensity = map(tidy_spikes,
                                       ~try(.x %>%
                                                group_by(block, stimulus) %>%
                                                summarise_at(vars(starts_with('cell')),
                                                             function(x) if(sum(x>0)>0){sum(x)/sum(x>0)}else{0}))
        )
    ) %>%
    select(-tidy_spikes) %>%
    mutate(error = map_lgl(spikes_count, ~class(.x)[1]=="try-error")) %>%
    filter(!error) %>% # drop downloading errors
    mutate(
        n_cells = map_dbl(spikes_count, ~try(ncol(.x)-2)),
        n_frames = map_dbl(stimuli_structure, ~try(sum(.x$n_frames)))
    ) %>%
    select(-error)

# choose donors observed on both sessions A and B
# to collect similar number of neurons per session
chosen_donors <- outcome_table  %>%
    select(imaging_depth, targeted_structure, donor_name) %>%
    group_by(imaging_depth, targeted_structure, donor_name) %>%
    summarise(count = n()) %>%
    filter(count == 2) %>%
    pluck('donor_name')

# select experiments according to chosen donors
clean_outcome_table <-  outcome_table %>%
    filter(donor_name %in% chosen_donors) %>%
    filter(!(imaging_depth == '175' & donor_name == '260936')) # drop donor because not observed at 175 session A

saveRDS(clean_outcome_table, file = 'data/spikes_outcome.rds')
