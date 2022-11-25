# parse time in ms
# it converts the column 'begin_frame' from a character to a POSIXct type
# it returns a column b_frame with the same content of begin_frame 
# but of POSIXct format to handle time operations (e.g., addition, subtractions)

# Parameters:
# begin_frame - character vector in the day:hour:minute:seconds:milliseconds format
# Returns:
# b_frame - POSIXct vector with hour:minute:seconds.milliseconds format

parse_time <- function(begin_frame){
  b_frame <- paste0(substr(begin_frame, 1, nchar(begin_frame) - 4), 
                    gsub(":", ".", substr(begin_frame, nchar(begin_frame) - 3, 
                                          nchar(begin_frame))))
  b_frame <- as.POSIXct(b_frame, format = "%H:%M:%OS")  
  return(b_frame)
}

# compute time duration
# takes in the begin_time column and computes the difference between row n+1, and row n
# returns difference between t1 and t1+1 (i.e., duration)
# Parameters:
# t1 = POSIXct vector with hour:minute:seconds.milliseconds format containing begin_frame times
# Returns:
# delta_t: POSIXct vector with hour:minute:seconds.milliseconds format containing frame duration
compute_time_duration <- function(t1){
  delta_t <- vector(mode = "numeric", length(t1))
  
  for (i in seq_len(length(delta_t))) {
    
    delta_t[i] <- as.numeric(difftime(t1[i + 1], t1[i])) 
    
  }
  return(delta_t)
}

# subset_shots
# subset dataset 'camera_shots' based on number of faces 'n'.
# the idea is to select the shots which contain clean clips of faces that matches the n number (i.e., without audience, film, or groupshots)
# it returns a dataframe 'faces_selected_filtered' which is the subset of 'camera_shots'.

# Parameters
# n = Integer vector; number of speakers
# camera_shots = dataframe containing the list of shots
# Returns:
# faces_selected_filtered = dataframe filtered out of instances whereby there were groupshots, audience or films

subset_shots <- function(n = 1, camera_shots){
  # not all rows are annotated. We convert NAs to -1 to allow unambiguous subset
  camera_shots <- camera_shots |> dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = -1)
  if (n == 1) {
    # select instances where there was one face or not faces at all
    # this allows us to have a dataset in which there are also no faces in addition to faces (for control purposes)
    faces_selected <- camera_shots[camera_shots$n_faces == 0 | camera_shots$n_faces == 1, ]
    # filter out frames in which there were groupshots, audience or films presented
    faces_selected_filtered <- faces_selected[faces_selected$groupshot != 1 & faces_selected$audience != 1 & faces_selected$film != 1, ]
    
  } else if (n > 1 & n <= 3) {
    # if more than one face, select n number of faces and filter out audience, groupshot and films
    faces_selected <- camera_shots[camera_shots$n_faces == n, ]
    faces_selected_filtered <- faces_selected[faces_selected$groupshot != 1 & 
                                                faces_selected$audience != 1 & 
                                                faces_selected$film != 1, ]
  }
  # append `n` for your info
  faces_selected_filtered$n <- n
  return(faces_selected_filtered)
}

# smallest_n_frame
# take the smallest n of frames -- this function is used within balance_dataset_by_frames
# Parameters
# temp = dataframe containing the columns 'face_combination', 'frames' and 'n'
# Returns:
# n__ = smallest frame found
smallest_n_frame <- function(df = temp){
  
  df |>
    # group by speakers
    dplyr::group_by(face_combination) |>
    # count frames by speakers
    dplyr::tally(frames) |>
    # filter out the speaker combination with the lowest frame number
    dplyr::filter(n == min(n)) -> min_frame
  
  n__ <- min_frame$n
  return(n__)
}

# balance dataset by frames
# matches the groups for the smallest `n_least_frequent_faces` of shots
# Parameters:
# df = dataframe of camera shots
# n_least_frequent_faces = integer, frequency of faces displayed
# Returns:
# minimal_clip_balanced = dataframe of shots matched for n_least_frequent_faces
balance_dataset_by_frames <- function(df, n_least_frequent_faces = 0){
  temp <- df |>
    dplyr::group_by(face_combination) |>
    dplyr::slice_sample(n = min(n_least_frequent_faces)) # matches the groups for the smallest n of shots
  
  minimal_clip_balanced <- temp
  minimal_clip_balanced$face_combination <- as.factor(minimal_clip_balanced$face_combination)
  return(minimal_clip_balanced)  
}

# count_face_combination
# count how many times a particular set of faces is present within the dataset
# returns a table sorted from max to min

# Parameters
# df = dataframe containing `face_1`, `face_2`, `face_3` columns
# Returns:
# table_df = table of face combinations sorted from max to min
count_face_combination <- function(df){
  df$face_combination <- paste0(df$face_1, df$face_2, df$face_3)
  
  table_df <- df |>
    dplyr::count(face_combination, sort = TRUE)
  return(table_df)
}

# select_most_frequent_faces
# Parameters:
# df = dataframe containing `face_1`, `face_2`, `face_3` columns
# write = boolean, to save output in a txt file
# list_shots_path = character, string referring to the txt file containing the list of shots
# out_name = character, output name
# n_unique_combinations = integer, number of unique faces in total
# balanced = boolean, to match number of frames displayed
# max_shots = integer, number of shots per face
# Returns:
# faces = list of shots with the faces selected ordered per frequency
select_most_frequent_faces <- function(df, write = TRUE, list_shots_path = "list_shots.txt", 
                                       out_name = "faces", n_unique_combinations = 1, 
                                       balanced = FALSE, max_shots){
  
  df$face_combination <- paste0(df$face_1, df$face_2, df$face_3)
  most_frequent_faces <- count_face_combination(df)[1:n_unique_combinations, 1]
  faces <- df[df$face_combination %in% most_frequent_faces & df$faces_in_background != 1, ]
  
  #  n_least_frequent_faces <- min(count_face_combination(faces)[1:n_unique_combinations,2])
  n_least_frequent_faces <- max_shots
  
  if (balanced) { # if we want all faces to be displayed the same amount of time
    faces <- balance_dataset_by_frames(df = faces, n_least_frequent_faces)
  } else {
    faces <- faces[sample(nrow(faces), n_least_frequent_faces), ]
  }
  
  if (write) {
    list_shots <- read.table(list_shots_path, header = F, sep = "\n")
    faces$abs_path <- vector(mode = "character", length = nrow(faces))
    
    for (x in seq_len(nrow(faces))) {
      faces$abs_path[x] <- list_shots[grep(paste0(faces$filename[x], ".mp4"), list_shots$V1), ]
    }
    
    write.table(paste0("file '", unique(faces$abs_path), "'"), file = paste0(out_name, ".txt"), row.names = F, col.names = F, quote = F)
  }
  return(faces)
}

# make clip
# generate costumised video clips by using ffmpeg
# number_of_speakers = integer vector, sequence of number of speakers over which to iterate
# camera_shots_annotation = dataframe containing the camera shots
# contemporaries = boolean, to select frames whereby there are multiple faces displayed at the same time (i.e., contemporaries) or not (i.e., 1 face displayed per frame)
# n_unique_combinations = integer, total amount of faces displayed in the video
# max_shots = integer, maximum number of shots per face
# Returns:
# (1) final_subset_faces = dataframe containing the list of shots that ffmpeg writes. These are saved as a txt file as well if writes = TRUE.
# (2) .mp4 files containing the video selected
make_clip <- function(number_of_speakers = seq_len(3), camera_shots_annotation, contemporaries = TRUE, n_unique_combination = 1, max_shots){
  
  if (contemporaries) {
    if (max(number_of_speakers) > 3) {
      stop("Error. number_of_speakers can't be > 3")
    }
    
    for (speaker in number_of_speakers) {
      out_name <- paste0(speaker, "_speaker_contemporary")
      
      # make subsets based on the number of speakers
      subset_n_face <- subset_shots(camera_shots = camera_shots_annotation, n = speaker)
      # Count frequencies of sets of faces displayed per time point and select most frequent face combination
      final_subset_faces <- select_most_frequent_faces(df = subset_n_face, write = TRUE, list_shots_path = 'list_shots.txt', out_name, n_unique_combinations = n_unique_combination, balanced = FALSE, max_shots)
      # concatenate the selected shots 
      system(paste0("ffmpeg -f concat -safe 0 -i ", out_name, ".txt -c copy ", out_name, ".mp4"))
      
    }
  }
  
  if (!contemporaries) {
    if (max(n_unique_combination) > 10) {
      stop("Error. number_of_speakers can't be > 10")
    }
    
    for (combination in n_unique_combination) {
      out_name <- paste0(combination, "_speaker_total")
      # make subsets based on the number of speakers
      subset_n_face <- subset_shots(camera_shots = camera_shots_annotation, n = 1)
      # Count frequencies of sets of faces displayed per time point and select most frequent face combination
      final_subset_faces <- select_most_frequent_faces(df = subset_n_face, write = TRUE, list_shots_path = "list_shots.txt", out_name, n_unique_combinations = combination, balanced = TRUE, max_shots)
      # concatenate the selected shots 
      system(paste0("ffmpeg -f concat -safe 0 -i ", out_name, ".txt -c copy ", out_name, ".mp4"))
      
    }
  }
  return(final_subset_faces)
}

# create_new_ref_audio
# create new reference audio
# Parameters:
# video_ref_segments = dataframe containing the reference
# audio_ref_segments = dataframe, models' output
# Returns:
# temp_df = dataframe, containing model's output re-referenced based on the reference
create_new_ref_audio<- function(video_ref_segments, audio_ref_segments){
  temp_df <- NULL
  
  for (subset_video in unique(video_ref_segments$video_name)) {
    video_ <- droplevels(subset(video_ref_segments, video_name == subset_video))
    for (subset_n in sort(video_$n)) {
      video_n <- video_[video_$n == subset_n, ]
      
      beg1 <- gsub("2022-10-24 ", "", video_n$b_frame)
      end1 <- gsub("2022-10-24 ", "", video_n$end_frame) # end of segment video
      
      for (x in seq_len(nrow(audio_ref_segments))) {
        beg2 <- audio_ref_segments[x, "tbeg"]
        end2 <- audio_ref_segments[x, "tend"]
        
        overlap_beg <- max(beg1, beg2) # later beginning is beginning of overlap
        overlap_end <- min(end1, end2) # earlier end is the end of overlap
        
        overlap_len <- as.numeric(difftime(as.POSIXct(overlap_end, format = "%H:%M:%OS"), as.POSIXct(overlap_beg, format = "%H:%M:%OS"))) 
        
        if ((overlap_len >= 0) & (!is.na(overlap_len))) { #if the segment are not overlapping this quantity is negative
          # because the earlier end is before the later beginning
          temp_df <- rbind(temp_df, cbind(video_n, audio_ref_segments[x, ]))
        }
      }
    }
    
  }
  return(temp_df)
}

# unpack_face_landmarks
# takes the column `face_landmarks` and puts everything contained within [] into a separate column with its own name
# Parameters:
# dataframe = self explainable
# Returns 
# dataframe = same dataframe as input, plus the new columns unpacked
unpack_face_landmarks <- function(dataframe){
  temp_face_landmarks <- dataframe |>
    dplyr::select(face_landmarks) 
  
  j <- temp_face_landmarks$face_landmarks     
  
  j <- stringr::str_extract_all(j, "(?<=\\[).+?(?=\\])", simplify = TRUE)
  
  for (x in seq_len(nrow(j))) {
    j[x, ] <- stringr::str_replace_all(j[x, ], stringr::fixed("["), "")
    j[x, ] <- stringr::str_squish(j[x, ])
    
  }
  
  j <- as.data.frame(j)
  old_names <- colnames(j)
  new_names <- paste0("fl", gsub("[^0-9.-]", "", old_names))
  colnames(j) <- new_names
  dataframe <- dplyr::bind_cols(dataframe, j)
  dataframe$face_landmarks <- NULL
  rm(j, temp_face_landmarks, new_names, old_names)
  return(dataframe)
}

# unpack_face_aus
# Takes the column `face_aus` and puts everything finds within [] into a separate column with its own name
# note that this is based on JAANET model only
# Parameters:
# dataframe = self explainable
# Returns:
# dataframe = same dataframe as in input, plus the aus columns unpacked
unpack_face_aus <- function(dataframe){
  stringr::str_replace_all(dataframe$face_aus, stringr::fixed("\n"), "") -> dataframe$face_aus_temp
  stringr::str_replace_all(dataframe$face_aus_temp, stringr::fixed("["), "") -> dataframe$face_aus_temp
  stringr::str_replace_all(dataframe$face_aus_temp, stringr::fixed("]"), "") -> dataframe$face_aus_temp
  
  dataframe |>
    tidyr::separate(face_aus_temp, c("AU01", "AU02", "AU04", "AU06", "AU07", "AU10", "AU12", "AU14", "AU15", "AU17", "AU23", "AU24"), "\\s+") -> dataframe
  dataframe$face_aus <- NULL
  return(dataframe)
}

# pivot_wider_video_annotation
# For each video, it repeats rows based on the number of frames and re-enumerate them starting from 0 (to match python)
# Paramenters:
# df = dataframe 
# Returns:
# annotation_wider =  dataframe elongated by n of frames and with additional column "frame" containing the number of frames starting from 0
pivot_wider_video_annotation <- function(df){
  videos <- unique(df$video_name)
  new_df <- NULL; annotation_wider <- NULL
  for (video in videos) {
    v <- droplevels(df[df$video_name == video, ])
    v <- v[order(v$n), ]
    frames <- v$frames + 1
    n_ <- v$n
    
    for (i in sort(n_)) {
      shot <- v[v$n == i, ]
      temp <- shot[rep(seq_len(nrow(shot)), each = frames[i]), ]
      new_df <- dplyr::bind_rows(new_df, temp)
      
    }
    len_df <- nrow(new_df[new_df$video_name == as.character(video), ]) 
    frame <- 0:(len_df - 1)
    new_df_ <- new_df[new_df$video_name == as.character(video), ] |> tibble::add_column(frame)
    annotation_wider <- dplyr::bind_rows(annotation_wider, new_df_)
  }
  return(annotation_wider)
  
}

# add_face_id_to_reference
# Matches the reference label (names of the speakers contained in the `face_1` column) with the mexca's output label (integers)
# Parameters:
# df = dataframe
# Returns:
# temp = dataframe containing the column `face_label` with the new mapping
add_face_id_to_reference <- function(df){
  videos <- unique(df$video_name)
  temp <- NULL
  
  for (video in videos) {
    v <- droplevels(df[df$video_name == video, ])
    
    face_mapping <- 0:(length(unique(v$face_1)) - 1)
    names(face_mapping) <- unique(v$face_1)
    
    # initialise column
    v$face_label <- -1
    
    for (r in seq_len(nrow(v))) {
      v[r, ]$face_label <- face_mapping[v[r, ]$face_1]
    }
    temp <- dplyr::bind_rows(temp, v)
  }
  return(temp)
}

## Optimal assignment:
# It matches labels from reference with the labels from model (i.e., mexca's output)
# Parameters:
# reference = sets of labels from reference
# model = sets of labels from model
# Returns:
# solution = named vector with correspondence between model's and reference's labels

min_matching <- function(reference, model) {
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
  assignment_matrix <- matrix(rep(-1, nC * nC), nrow = nC)
  colnames(assignment_matrix) <- c(0:(nC - 1))
  rownames(assignment_matrix) <- c(0:(nC - 1))
  
  for (i in 0:(nC - 1)) {
    tupelClusterI <- tupel[reference == i]
    solRowI <- sapply(0:(nC - 1), function(i, clusterIDsB, tupelA_I) {
      nA_I <- length(tupelA_I)  # number of elements in cluster I
      tupelB_I <- tupel[clusterIDsB == i]
      nB_I <- length(tupelB_I)
      nTupelIntersect <- length(intersect(tupelA_I, tupelB_I))
      return((nA_I - nTupelIntersect) + (nB_I - nTupelIntersect))
    }, model, tupelClusterI)
    assignment_matrix[i + 1, ] <- solRowI
  }
  solution <- lpSolve::lp.assign(assignment_matrix, direction = "min")$solution
  colnames(solution) <- c(0:(nC - 1))
  rownames(solution) <- c(0:(nC - 1))
  return(solution)
}

