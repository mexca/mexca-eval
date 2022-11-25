# exponentialFun
# function that creates a distribution of length k, max frequency a, and slope b
# Parameters:
# k = integer, length of the sample
# a = double, max frequency
# b = double, slope of the function (i.e., exponential decay)
# Returns:
# exponential distribution of frequencies of occurrence rounded to single digits
exponentialFun <- function(k, a, b) {return(round(a * exp(-b * k)))}

# make_wav
# create list of audio files based on parameters saved as `.csv` in the list_audio\unbalanced\ folder
# Parameters:
# number_of_speakers = integer, n of speakers in total per audio file
# audio_annotation = dataframe, annotation provided by the project partner
# max_duration = double, max time duration per speaker
# balanced = boolean, whether the speakers have to speak the same amount of time (TRUE) or follow an exponential distribution (FALSE)
# outfolder = character, folder whereby you want to store the list of audio files. 
# Returns:
# wav_file = dataframe, list of all wav files together
make_wav <- function(number_of_speakers = 2, audio_annotation, max_duration, balanced = T, outfolder) {
  
  n_speakers <- sample(unique(audio_annotation$name), number_of_speakers)
  sub_n_speakers <- audio_annotation[audio_annotation$name %in% n_speakers,]
  
  wav_file <- NULL
  
  timeDistribution <- rep(max_duration, number_of_speakers)
  
  if(balanced == F) {
    k <- c(0:number_of_speakers); a = max_duration; b = .6
    timeDistribution <- NULL
    for (i in 1:length(k)) {
      timeDistribution[i]<- exponentialFun(k[i], a, b)
    }
  }
  
  table_duration <- aggregate(tdur ~ name, data = sub_n_speakers, sum)
  ordered_speakers <- table_duration$name
  
  for (n in 1:length(n_speakers)) {
    speaker_subset <- sub_n_speakers[sub_n_speakers$name == ordered_speakers[n], ]
    speaker_tdur <- speaker_subset$tdur
    max_duration_ <- timeDistribution[n]
    # select rows based on max duration
    idx <- which(cumsum(speaker_tdur) <= max_duration_)
    
    if (length(idx) == 0) {
      wav_file <- dplyr::bind_rows(wav_file, speaker_subset[1, ])
    }
    
    wav_file <- dplyr::bind_rows(wav_file, speaker_subset[idx, ])
    wav_file$max_duration <- max_duration
    wav_file$n_speaker <- number_of_speakers
    wav_file$balanced <- balanced
    write.csv(wav_file, 
              file.path(
                outfolder, 
                paste0("speaker_", 
                       number_of_speakers, 
                       "_duration_", 
                       max_duration, 
                       "_", 
                       ifelse(balanced, "balanced", "unbalanced"), 
                       ".csv")
              )
    )
  }
  return(wav_file)
}

# split_select_wav
# pick, cut, concatenate and save audio files in the balanced/unbalanced folders
# Parameters:
# wav_dataset = dataframe output of make_wav function
split_select_wav <- function(wav_dataset) {
  seq_speakers <- unique(wav_dataset$n_speaker)
  seq_duration <- unique(wav_dataset$max_duration)
  balance <- unique(wav_dataset$balanced)
  
  dir.create(paste0(ifelse(balance, "balanced", "unbalanced")))
  
  for (i in seq_speakers) {
    dir.create(
      paste0(ifelse(balance, "balanced", "unbalanced"), "/Speaker_", i)
    )
  }
  
  for (x in seq_speakers) {
    for (t in seq_duration) {
      wav_subset <- wav_dataset[wav_dataset$max_duration == t & wav_dataset$n_speaker == x, ]
      
      wav_subset$n <- seq_len(nrow(wav_subset))
      outfolder <- paste0(
        ifelse(balance, "balanced", "unbalanced"), 
        "/Speaker_", x
      )
      
      for (row in wav_subset$n) {
        outname <- paste0("Speaker_", wav_subset[wav_subset$n == row, ]$name, "_duration_", t)
        system(
          paste0(
            "ffmpeg -ss ", wav_subset[wav_subset$n == row, ]$tbeg, " -to " , 
            wav_subset[wav_subset$n == row,]$tend, " -i ", 
            wav_subset[wav_subset$n == row, ]$audio_file, 
            ".wav ", outfolder, "/", outname, "_", row, ".wav"
          )
        )
      }
    }
  }
}

# n <- seq_len(length(unique(ref_rttmFile$name)))
# max_duration <- c(40, 60, 120, 180, 300, 480, 600)
# speakers_names <- unique(ref_rttmFile$name)

# concatenate_wav
# Uses ffmpeg to concatenate wav files based on number of speakers, max duration and whether speakers' max duration is balanced or not
# wav files are saved into `output_wav/balanced or output_wav/unbalanced` folder. List of audio files concatenated are stored in `list_audio/balanced or list_audio/unbalanced`  folder
# Parameters:
# speakers_name = character vector, unique names of the reference (i.e., unique(ref_rttmFile$name))
# max_duration = integer vector, max time duration of the most frequent speaker
# n = integer vector, unique sequence from 1 to max number of speakers' names.
# balance = boolean, whether speakers max duration has to be matched across speakers
# Returns:
# Folders `output_wav` and `list_audio` with their respective balanced/unbalanced folders. They contain the lists (.txt) of audio files that have been concatenated and the actual wav files (.wav)
concatenate_wav <- function(speakers_names, max_duration, n, balance = T) {
  balance_ <- ifelse(balance, "balanced", "unbalanced")
  
  out_names <- vector(mode = "character")
  for (i in n) {
    for (t in max_duration) {
      path_folder <- file.path(balance_, paste0("Speaker_", n[i]))
      outfolder <- list.files(file.path(getwd(), path_folder), full.names = TRUE)
      speaker_t_files <- outfolder[grepl(paste0("_", t, "_"), outfolder)]
      temp <- str_split(speaker_t_files, pattern = "_", simplify = TRUE)
      as.data.frame(temp) -> temp
      patterns_order <- mixedsort(temp[, ncol(temp)])
      temp <- temp[mixedorder(temp[, ncol(temp)]), ]
      speaker_path <- file.path(
        getwd(), 
        path_folder, 
        paste("Speaker", temp[, 3], temp[, 4], temp[, 5], temp[, 6], sep = "_")
      )
      speaker_t_files <- paste0("file '", speaker_path, "'")
      
      # saves list of audio files that have been concatenated into `list_audio`
      dir.create("list_audio")
      dir_out_list <- paste0(
        "list_audio", 
        paste0("/", ifelse(balance, "balanced", "unbalanced")
        )
      )
      dir.create(dir_out_list)
      
      out_path <- file.path(
        getwd(), 
        dir_out_list, 
        paste0("list_audio_", i, "_", t, ".txt")
      )
      out_names <- append(out_names, out_path)
      write.table(speaker_t_files, out_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
    
  }
  
  # saves wav files in output_wav
  dir.create("output_wav")
  dir_out <- paste0(
    "output_wav", 
    paste0("/", ifelse(balance, "balanced", "unbalanced")
    )
  )
  
  dir.create(dir_out)
  # concatenate audio with ffmpeg
  for (y in out_names) {
    temp_y <- str_split(y, .Platform$file.sep, simplify = TRUE)
    char_y <- temp_y[, ncol(temp_y)]
    out_name <- file.path(
      dir_out, 
      paste0(substr(char_y, 1, nchar(char_y) - 4), "_", 
             ifelse(balance, "balanced", "unbalanced")
      )
    )
    # ffmpeg to concatenate files
    system(
      paste0("ffmpeg -f concat -safe 0 -i ", y, " -c copy ", out_name, ".wav")
    )
  }
}

# calculate_overlap
# computes the amount of overlap in seconds between the reference's and the model's speakers
# Parameters:
# speaker_ref_segments = dataframe, reference in rttm format (i.e., with `tbeg`, `tdur` columns)
# speaker_pred_segments = dataframe, model's output in rttm format (i.e., with `tbeg`, `tdur` columns)
# Returns:
# tot_overlap = numeric vector, total amount of overlap in seconds
calculate_overlap<- function(speaker_ref_segments, speaker_pred_segments) {
  
  tot_overlap = 0
  
  for (segment1 in seq_len(nrow(speaker_ref_segments))) {
    for (segment2 in seq_len(nrow(speaker_pred_segments))) {
      beg1 <- speaker_ref_segments[segment1, "tbeg"]
      dur1 <- speaker_ref_segments[segment1, "tdur"]
  
      end1 = beg1 + dur1 #calculates end of segment 1
      
      beg2 <- speaker_pred_segments[segment2, "tbeg"]
      dur2 <- speaker_pred_segments[segment2, "tdur"]
      
      end2 = beg2 + dur2 # calculate end of segment 2
      
      overlap_beg = max(beg1,beg2) # later beginning is beginning of overlap
      overlap_end = min(end1,end2) # earlier end is the end of overlap
      
      overlap_len = overlap_end - overlap_beg
      
      #if the segments are not overlapping this quantity (i.e., overlap_len) is negative
      # because the earlier end is before the later beginning
      if (overlap_len >= 0) { 
        tot_overlap = tot_overlap + overlap_len
      }
    }
  }
  return(tot_overlap)
}

# compute_recall_model
# computes recall of model's output from true positives and false negatives
# Parameters:
# false_n = double, false negative
# true_p = double, true positive
# Returns:
# Recall = double, recall
compute_recall_model = function(false_n, true_p) {
  
  Recall = (true_p$seconds) /
    sum(true_p$seconds,
        false_n$seconds)
  
  return(Recall)
}

# compute_recall
# computes recall from confusion matrix
# Parameters:
# matrix_ = matrix, confusion matrix
# speaker = character, unique name of the speaker
# Returns:
# Recall = double, recall of the speaker
compute_recall = function(matrix_, speaker) {
  conf_matrix <- as.data.frame.matrix(matrix_) 
  
  true_positive_1_1 <- conf_matrix |>
    rownames_to_column("true_label") |>
    pivot_longer(-c(true_label), names_to = "pred_label", values_to = "seconds")|>
    filter(true_label == pred_label) |>
    filter(true_label == all_of(speaker)) |>
    select(seconds)|>
    as.data.frame()
  
  false_negative_1_0 <- conf_matrix |>
    rownames_to_column("true_label") |>
    pivot_longer(-c(true_label), names_to = "pred_label", values_to = "seconds")|>
    filter(true_label != pred_label) |>
    filter(pred_label == all_of(speaker)) |>
    summarise(seconds = sum(seconds))|>
    as.data.frame()
  
  Recall = compute_recall_model(false_negative_1_0, true_positive_1_1)
  
  return(Recall)
}

# compute_precision
# computes model's precision from confusion matrix
# Parameters:
# matrix_ = matrix, confusion matrix
# speaker = character, unique name of the speaker
# Returns:
# Precision = double, precision of the speaker
compute_precision = function(matrix_, speaker){
  conf_matrix <- as.data.frame.matrix(matrix_) 
  
  true_positive_1_1 <- conf_matrix |>
    rownames_to_column("true_label") |>
    pivot_longer(-c(true_label), names_to = "pred_label", values_to = "seconds")|>
    filter(true_label == pred_label) |>
    filter(true_label == all_of(speaker)) |>
    select(seconds)|>
    as.data.frame()
  
  false_positive_0_1 <- conf_matrix |> #the row except the diagonal cell
    rownames_to_column("absent_label") |>
    select(-all_of(speaker)) |>
    pivot_longer(-c(absent_label), names_to = "pred_label", values_to = "seconds")|>
    filter(absent_label != pred_label)|>
    filter(absent_label == all_of(speaker))|>
    summarise(seconds = sum(seconds)) |>
    as.data.frame()
  
  Precision <- compute_precision_model(false_positive_0_1, true_positive_1_1)
  return(Precision)
  
}

# compute_precision_model
# computes model's precision from true and false positive 
# false_p = double, false positives
# true_p = double, true positives
# Returns:
# Precision = double, precision
compute_precision_model = function(false_p, true_p){
  
  Precision = (true_p$seconds) /
    sum(true_p$seconds,
        false_p$seconds)
  
  return(Precision)
}

# compute_f1
# computes f1 score of the model per speaker
# Parameters:
# matrix_ = matrix, model's confusion matrix
# speaker = character, unique name of the reference speaker
# Returns:
# f1_score = double, f1 score for that particular speaker
compute_f1 <- function(matrix_, speaker) {
  conf_matrix <- as.data.frame.matrix(matrix_) 
  s <- speaker
  
  recall <- compute_recall(conf_matrix, s)
  precision <- compute_precision(conf_matrix, s)
  
  f1_score <- 2 * (recall * precision) / (recall + precision)
  return(f1_score)
}