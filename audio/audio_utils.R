exponentialFun <- function(k, a, b) {return(round(a*exp(-b*k)))}

make_wav <- function(number_of_speakers = 2, audio_annotation, max_duration, balanced = T, outfolder){
  
  n_speakers <- sample(unique(audio_annotation$name), number_of_speakers)
  sub_n_speakers <- audio_annotation[audio_annotation$name %in% n_speakers,]
  
  wav_file <- NULL
  
  timeDistribution <- rep(max_duration, number_of_speakers)
  
  if(balanced == F) {
    k <- c(0:number_of_speakers); a = max_duration; b = .6
    
    timeDistribution <- NULL
    for (i in 1:length(k)){
      timeDistribution[i]<- exponentialFun(k[i], a, b)
    }
    
  }
  table_duration <- aggregate(tdur ~ name, data = sub_n_speakers, sum)
  ordered_speakers <- table_duration$name
  
  for (n in 1:length(n_speakers)){
    speaker_subset <- sub_n_speakers[sub_n_speakers$name == ordered_speakers[n],]
    speaker_tdur <- speaker_subset$tdur
    
    max_duration_ <- timeDistribution[n]
    
    # select rows based on max duration
    idx <- which(cumsum(speaker_tdur) <= max_duration_)
    
    if(length(idx) == 0){
      wav_file <- dplyr::bind_rows(wav_file, speaker_subset[1,])
    }
    
    wav_file <- dplyr::bind_rows(wav_file, speaker_subset[idx,])
    wav_file$max_duration <- max_duration
    wav_file$n_speaker <- number_of_speakers
    wav_file$balanced <- balanced
    
    write.csv(wav_file, file.path(outfolder, paste0("speaker_", number_of_speakers, "_duration_", max_duration, "_",ifelse(balanced == T, "balanced","unbalanced"), ".csv")))
  }

  
  return(wav_file)
}

split_select_wav <- function(wav_dataset){
  seq_speakers <- unique(wav_dataset$n_speaker)
  seq_duration <- unique(wav_dataset$max_duration)
  balance <- unique(wav_dataset$balanced)
  
  dir.create(paste0(ifelse(balance == T, "balanced", "unbalanced")))
  
  for(i in seq_speakers){
    dir.create(paste0(ifelse(balance == T, "balanced", "unbalanced"), "/Speaker_", i))
  }
  
  for(x in seq_speakers){
    for(t in seq_duration){
      wav_subset <- wav_dataset[wav_dataset$max_duration == t & wav_dataset$n_speaker == x,]
      
      wav_subset$n <- 1:nrow(wav_subset)
      outfolder <- paste0(ifelse(balance == T, "balanced", "unbalanced"), "/Speaker_", x)
      
      for(row in wav_subset$n){
        outname <- paste0("Speaker_", wav_subset[wav_subset$n == row,]$name, "_duration_", t)
        system(paste0("ffmpeg -ss ", wav_subset[wav_subset$n == row,]$tbeg, " -to " , wav_subset[wav_subset$n == row,]$tend, " -i ", wav_subset[wav_subset$n == row,]$audio_file, ".wav ", outfolder,"/", outname, "_",row,".wav"))
      }
      
    }
  }
  
}
 
# n <- seq_len(length(unique(ref_rttmFile$name)))
# max_duration <- c(40, 60, 120, 180, 300, 480, 600)
# speakers_names <- unique(ref_rttmFile$name)

concatenate_wav <- function(speakers_names, max_duration, n, balance = T){
  
  balance_ <- ifelse(balance == T, "balanced", "unbalanced")
  
  out_names <- vector(mode = "character")
  for(i in n){
    for(t in max_duration){
      path_folder <- file.path(balance_, paste0("Speaker_", n[i]))
      outfolder <- list.files(file.path(getwd(), path_folder), full.names = T)
      speaker_t_files <- outfolder[grepl(paste0("_",t,"_"),outfolder)]
      temp <- str_split(speaker_t_files, pattern = "_", simplify = T)
      as.data.frame(temp) -> temp
      patterns_order <- mixedsort(temp[,ncol(temp)])
      temp <- temp[mixedorder(temp[,ncol(temp)]),]
      speaker_path <- file.path(getwd(), path_folder, paste("Speaker", temp[,3], temp[,4], temp[,5], temp[,6], sep = "_"))
      speaker_t_files <- paste0("file '", speaker_path, "'")
      
      dir.create("list_audio")
      dir_out_list <- paste0("list_audio", paste0("/",ifelse(balance == T, "balanced", "unbalanced")))
      dir.create(dir_out_list)
      
      out_path <- file.path(getwd(), dir_out_list, paste0("list_audio_",i,"_",t,".txt"))
      out_names <- append(out_names, out_path)
      write.table(speaker_t_files, out_path, row.names = F, col.names = F, quote = F)
    }
    
  }
  
  dir.create("output_wav")
  dir_out <- paste0("output_wav", paste0("/",ifelse(balance == T, "balanced", "unbalanced")))
  dir.create(dir_out)
  # concatenate audio
  for(y in out_names){
    temp_y <- str_split(y, .Platform$file.sep, simplify = T)
    char_y <- temp_y[, ncol(temp_y)]
    out_name <- file.path(dir_out, paste0(substr(char_y,1, nchar(char_y)-4), "_", ifelse(balance == T, "balanced", "unbalanced")))
    system(paste0("ffmpeg -f concat -safe 0 -i ", y," -c copy ", out_name,".wav"))
  }
  
}

calculate_overlap<- function(speaker_ref_segments, speaker_pred_segments){
  
  tot_overlap = 0
  
  for (segment1 in 1:nrow(speaker_ref_segments)){
    for (segment2 in 1:nrow(speaker_pred_segments)){
      beg1 <- speaker_ref_segments[segment1, "tbeg"]
      dur1 <- speaker_ref_segments[segment1, "tdur"]
      
      end1 = beg1 + dur1 #calculates end of segment 1
      
      beg2 <- speaker_pred_segments[segment2, "tbeg"]
      dur2 <- speaker_pred_segments[segment2, "tdur"]
      
      end2 = beg2 + dur2 # calculate end of segment 2
      
      overlap_beg = max(beg1,beg2) # later beginning is beginning of overlap
      overlap_end = min(end1,end2) # earlier end is the end of overlap
      
      overlap_len = overlap_end - overlap_beg
      
      if (overlap_len >= 0){ #if the segment are not overlapping this quantity is negative
        # because the earlier end is before the later beginning
        tot_overlap = tot_overlap + overlap_len
      }
    }
  }
  return(tot_overlap)
}


compute_recall_model = function(false_n, true_p){
  
  Recall = (true_p$seconds) /
    sum(true_p$seconds,
        false_n$seconds)
  
  return(Recall)
}



compute_precision_model = function(false_p, true_p){
  
  Precision = (true_p$seconds) /
    sum(true_p$seconds,
        false_p$seconds)
  
  return(Precision)
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

compute_f1 <- function(matrix_, speaker){
  conf_matrix <- as.data.frame.matrix(matrix_) 
  
  true_positive_1_1 <- conf_matrix |>
    rownames_to_column("true_label") |>
    pivot_longer(-c(true_label),names_to = "pred_label", values_to = "seconds")|>
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
  
  false_negative_1_0 <- conf_matrix |>
    rownames_to_column("true_label") |>
    pivot_longer(-c(true_label), names_to = "pred_label", values_to = "seconds")|>
    filter(true_label != pred_label) |>
    filter(pred_label == all_of(speaker)) |>
    summarise(seconds = sum(seconds))|>
    as.data.frame()
  
  true_negative_0_0 <- conf_matrix|> # all the rest except that speaker
    rownames_to_column("absent_label")|>
    select(-all_of(speaker)) |>
    filter(absent_label != all_of(speaker)) |>
    pivot_longer(-c(absent_label), names_to = "pred_label", values_to = "seconds")|>
    summarise(seconds = sum(seconds))|>
    as.data.frame()
  
  
  recall <- compute_recall_model(false_negative_1_0, true_positive_1_1)
  precision <- compute_precision_model(false_positive_0_1, true_positive_1_1)
  
  f1_score <- 2 * (recall * precision) / (recall + precision)
  return(f1_score)
}

lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}