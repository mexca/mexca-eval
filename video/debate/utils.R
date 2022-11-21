# parse time in ms
# convert the column 'begin_frame' from a character to a POSIXct type to handle time operations (e.g., addition, subtractions)
# returns a column b_frame with the same content of begin_frame but of POSIXct format

parse_time <- function(df){
  b_frame <- paste0(substr(df$begin_frame, 1, nchar(df$begin_frame)-4), 
                    gsub(":", ".", substr(df$begin_frame,nchar(df$begin_frame)-3, 
                                          nchar(df$begin_frame))))
  b_frame <- as.POSIXct(b_frame, format = "%H:%M:%OS")  
  return(b_frame)
}

# compute time duration
# takes in the begin_time column and computes the difference between row n+1, and row n
# returns delta_t

compute_time_duration <- function(t1){
  delta_t <- vector(mode = "numeric", length(t1))
  
  for (i in 1:length(delta_t)){
    delta_t[i] <- as.numeric(difftime(t1[i+1], t1[i])) 
  }
  return(delta_t)
}

# subset_shots
# subset dataset 'camera_shots' based on number of faces 'n'.
# the idea is to select the shots which contain clean clips of faces that matches the n number (i.e., without audience, film or groupshots)
# it returns a dataframe 'faces_selected_filtered' which is the subset of 'camera_shots'.

subset_shots <- function(n = 1, camera_shots) {
  # not all rows are annotated. We convert NAs to -1 to allow unambiguous subset
  camera_shots <- camera_shots |> dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = -1)
  if( n == 1) {
    faces_selected <- camera_shots[camera_shots$n_faces == 0 | camera_shots$n_faces == 1,]
    faces_selected_filtered <- faces_selected[faces_selected$groupshot != 1 & faces_selected$audience != 1 & faces_selected$film != 1,]
    
  } else if (n > 1 & n <= 3){
    faces_selected <- camera_shots[camera_shots$n_faces == n,]
    faces_selected_filtered <- faces_selected[faces_selected$groupshot != 1 & 
                                                faces_selected$audience != 1 & 
                                                faces_selected$film != 1,]
  }
  faces_selected_filtered$n <- n
  return(faces_selected_filtered)
}

# take the smallest n of frames
smallest_n_frame <- function(df = temp){
  df |>
    dplyr::group_by(face_combination) |>
    dplyr::tally(frames) |>
    dplyr::filter(n == min(n)) -> min_frame
  
  return(min_frame$n)
}

# select the same number of shots (based on the minimum) and appends a frames 
balance_dataset_by_frames <- function(df, n_least_frequent_faces = 0){
  temp <- df |>
    dplyr::group_by(face_combination) |>
    dplyr::slice_sample(n = min(n_least_frequent_faces)) # matches the groups for the smalles n of shots
  
  # -- commented out as I was unable to trim videos based on frame number -- will come back on this later on if necessary
  # # create empty df  
  # minimal_clip <- data.frame(matrix(ncol = ncol(temp)+1, nrow =0))
  # colnames(minimal_clip) <- colnames(temp)
  # 
  # # create empty vector for appending frames real number 
  # frames_column <- vector(mode = 'integer', length = 0)
  # 
  # for(x in 1:nrow(temp)){
  #   n_frame_rows <- as.numeric(temp[x, c("frames")])
  #   minimal_clip <- rbind(minimal_clip, temp[rep(x, n_frame_rows),])
  #   frames_column <- append(frames_column, seq_len(n_frame_rows))
  # }
  # 
  # minimal_clip$frames <- frames_column; rm(frames_column) # remove this vector as not necessary anymore
  
  
  #min_frame <- smallest_n_frame(df = temp)
  
  #  minimal_clip_balanced <- minimal_clip |>
  #    dplyr::group_by(face_combination) |>
  #    dplyr::slice_sample(n = min_frame) # balance the dataset now having the same number of frame (i.e., same time duration)
  minimal_clip_balanced <- temp
  minimal_clip_balanced$face_combination <- as.factor(minimal_clip_balanced$face_combination)
  return(minimal_clip_balanced)  
}

# count_face_combination
# count how many times a particular set of faces is present within the dataset
# returns a table sorted from max to min
count_face_combination <- function(df){
  df$face_combination <- paste0(df$face_1, df$face_2, df$face_3)
  
  df |>
    dplyr::count(face_combination, sort = TRUE)
}

# select_most_frequent_faces
# returns a list of shots with the most frequent faces displayed
# write = T saves them in a txt file
select_most_frequent_faces <- function(df, write = TRUE, list_shots_path = 'list_shots.txt', 
                                       out_name = 'faces', n_unique_combinations = 1, 
                                       balanced = FALSE, max_shots){
  
  df$face_combination <- paste0(df$face_1, df$face_2, df$face_3)
  most_frequent_faces <- count_face_combination(df)[1:n_unique_combinations,1]
  faces <- df[df$face_combination %in% most_frequent_faces & df$faces_in_background != 1,]
  
  #  n_least_frequent_faces <- min(count_face_combination(faces)[1:n_unique_combinations,2])
  n_least_frequent_faces <- max_shots
  
  if(balanced){ # if we want all faces to be displayed the same amount of time
    faces <- balance_dataset_by_frames(df = faces, n_least_frequent_faces)
  } else {
    faces <- faces[sample(nrow(faces), n_least_frequent_faces), ]
  }
  
  
  
  
  
  if(write){
    list_shots <- read.table(list_shots_path, header = F)
    faces$abs_path <- vector(mode = "character", length = nrow(faces))
    
    for(x in 1:nrow(faces)){
      faces$abs_path[x] <- list_shots[grep(paste0(faces$filename[x],".mp4"), list_shots$V1),]
    }
    
    write.table(paste0("file '",unique(faces$abs_path),"'"), file = paste0(out_name, ".txt"), row.names = F, col.names = F, quote = F)
    
  }
  
  return(faces)
}

# make clip
make_clip <- function(number_of_speakers = seq_len(3), camera_shots_annotation, contemporaries = TRUE, n_unique_combination = 1, max_shots){
  
  if(contemporaries){
    if(max(number_of_speakers)>3){
      stop("Error. number_of_speakers can't be > 3")
    }
    
    for(speaker in number_of_speakers){
      out_name <- paste0(speaker,"_speaker_contemporary")
      
      # make subsets based on the number of speakers
      subset_n_face <- subset_shots(camera_shots = camera_shots_annotation, n = speaker)
      # Count frequencies of sets of faces displayed per time point and select most frequent face combination
      final_subset_faces <- select_most_frequent_faces(df = subset_n_face, write = TRUE, list_shots_path = 'list_shots.txt', out_name, n_unique_combinations = n_unique_combination, balanced = F, max_shots)
      # concatenate the selected shots 
      system(paste0("ffmpeg -f concat -safe 0 -i ", out_name,".txt -c copy ", out_name,".mp4"))
      
    }
  }
  
  if(contemporaries == FALSE ){
    if(max(n_unique_combination)>10){
      stop("Error. number_of_speakers can't be > 10")
    }
    
    for(combination in n_unique_combination){
      out_name <- paste0(combination,"_speaker_total")
      # make subsets based on the number of speakers
      subset_n_face <- subset_shots(camera_shots = camera_shots_annotation, n = 1)
      # Count frequencies of sets of faces displayed per time point and select most frequent face combination
      final_subset_faces <- select_most_frequent_faces(df = subset_n_face, write = TRUE, list_shots_path = 'list_shots.txt', out_name, n_unique_combinations = combination, balanced = TRUE, max_shots)
      # concatenate the selected shots 
      system(paste0("ffmpeg -f concat -safe 0 -i ", out_name,".txt -c copy ", out_name,".mp4"))
      
    }
  }
  
}


create_new_ref_audio<- function(video_ref_segments, audio_ref_segments){
  temp_df <- NULL
  
  for(subset_video in unique(video_ref_segments$video_name)) {
    video_ <- droplevels(subset(video_ref_segments, video_name == subset_video))
    for(subset_n in sort(video_$n)){
      video_n <- video_[video_$n == subset_n,]
      
      beg1 <- gsub("2022-10-24 ","",video_n$b_frame)
      end1 <- gsub("2022-10-24 ","",video_n$end_frame) # end of segment video
      
      for(x in 1:nrow(audio_ref_segments)){
        beg2 <- audio_ref_segments[x, "tbeg"]
        end2 <- audio_ref_segments[x, "tend"]
        
        overlap_beg = max(beg1,beg2) # later beginning is beginning of overlap
        overlap_end = min(end1,end2) # earlier end is the end of overlap
        
        overlap_len <- as.numeric(difftime(as.POSIXct(overlap_end, format = "%H:%M:%OS"), as.POSIXct(overlap_beg, format = "%H:%M:%OS"))) 
        
        if ((overlap_len >= 0) & (!is.na(overlap_len))){ #if the segment are not overlapping this quantity is negative
          # because the earlier end is before the later beginning
          temp_df <- rbind(temp_df, cbind(video_n, audio_ref_segments[x, ]))
        }
      }
    }
    
  }
  return(temp_df)
}


unpack_face_landmarks <- function(dataframe){
  temp_face_landmarks <- dataframe |>
    dplyr::select(face_landmarks) 
  
  j <- temp_face_landmarks$face_landmarks     
  
  j<- stringr::str_extract_all(j, "(?<=\\[).+?(?=\\])", simplify = TRUE)
  
  for(x in 1:nrow(j)){
    j[x,] <- stringr::str_replace_all(j[x,],stringr::fixed("["), "")
    j[x,] <- stringr::str_squish(j[x,])
    
  }
  
  j<- as.data.frame(j)
  old_names <- colnames(j)
  new_names <- paste0("fl",gsub("[^0-9.-]", "", old_names))
  colnames(j) <- new_names
  dataframe <- dplyr::bind_cols(dataframe,j)
  dataframe$face_landmarks <- NULL
  rm(j, temp_face_landmarks, new_names, old_names)
  return(dataframe)
}

unpack_face_aus <- function(dataframe){
  stringr::str_replace_all(dataframe$face_aus, stringr::fixed("\n"), "") -> dataframe$face_aus_temp
  stringr::str_replace_all(dataframe$face_aus_temp, stringr::fixed("["), "") -> dataframe$face_aus_temp
  stringr::str_replace_all(dataframe$face_aus_temp, stringr::fixed("]"), "") -> dataframe$face_aus_temp
  
  dataframe |>
    tidyr::separate(face_aus_temp, c("AU01", "AU02", "AU04", "AU06", "AU07", "AU10", "AU12", "AU14", "AU15", "AU17", "AU23", "AU24"), "\\s+") -> dataframe
  dataframe$face_aus <- NULL
  return(dataframe)
}

pivot_wider_video_annotation <- function(df){
  videos <- unique(df$video_name)
  new_df <- NULL; annotation_wider <- NULL
  for(video in videos){
    v <- droplevels(df[df$video_name == video,])
    v <- v[order(v$n),]
    frames <- v$frames + 1
    n_ <- v$n
    
    for(i in sort(n_)){
      shot <- v[v$n == i, ]
      temp <- shot[rep(seq_len(nrow(shot)), each = frames[i]), ]; 
      new_df <- dplyr::bind_rows(new_df, temp)
      
    }
    len_df <- nrow(new_df[new_df$video_name == as.character(video),]); 
    frame <- 0:(len_df-1)
    new_df_ <- new_df[new_df$video_name == as.character(video),] |> tibble::add_column(frame)
    annotation_wider <- dplyr::bind_rows(annotation_wider, new_df_)
  }
  return(annotation_wider)
  
}

add_face_id_to_reference <- function(df){
  videos <- unique(df$video_name)
  temp <- NULL
  
  for(video in videos){
    v <- droplevels(df[df$video_name == video,])
    
    face_mapping <- 0:(length(unique(v$face_1))-1)
    names(face_mapping) <- unique(v$face_1)
    
    # initialise column
    v$face_label <- -1
    
    for(r in 1:nrow(v)){
      v[r,]$face_label <- face_mapping[v[r,]$face_1]
    }
    temp <- dplyr::bind_rows(temp, v)
  }
  
  return(temp)
}

## Optimal assignment:
# labels from reference will be matched on the labels from model 
minMatching <- function(reference, model) {
  idsA <- unique(reference)  # distinct cluster ids in a
  idsB <- unique(model)  # distinct cluster ids in b
  nA <- length(reference)  # number of instances in a
  nB <- length(model)  # number of instances in b
  if (length(idsA) != length(idsB) || nA != nB) {
    stop("number of cluster or number of instances do not match")
  }
  
  nC <- length(idsA)
  tupel <- c(1:nA)
  
  # computing the distance matrix
  assignmentMatrix <- matrix(rep(-1, nC * nC), nrow = nC)
  colnames(assignmentMatrix) <- c(0:(nC-1))
  rownames(assignmentMatrix) <- c(0:(nC-1))
  
  for (i in 0:(nC-1)) {
    tupelClusterI <- tupel[reference == i]
    solRowI <- sapply(0:(nC-1), function(i, clusterIDsB, tupelA_I) {
      nA_I <- length(tupelA_I)  # number of elements in cluster I
      tupelB_I <- tupel[clusterIDsB == i]
      nB_I <- length(tupelB_I)
      nTupelIntersect <- length(intersect(tupelA_I, tupelB_I))
      return((nA_I - nTupelIntersect) + (nB_I - nTupelIntersect))
    }, model, tupelClusterI)
    assignmentMatrix[i+1, ] <- solRowI
  }
  
  
  solution <- lpSolve::lp.assign(assignmentMatrix, direction = "min")$solution
  colnames(solution) <- c(0:(nC-1))
  rownames(solution) <- c(0:(nC-1))
  return(solution)
  
  # optimization
  #result <- solve_LSAP(assignmentMatrix, maximum = TRUE)
  #result = result-1
  #attr(result, "assignmentMatrix") <- assignmentMatrix
  #return(result)
  
  #example
  # minMatching(
  #   c(rep(0,5), rep(1,5)), 
  #   c(rep(0,5), rep(1,5))
  # )
  # minMatching(
  #   c(rep(0,5), rep(1,5)), 
  #   c(rep(1,4), rep(0,1), rep(1,5))
  # )
  
 
  
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

