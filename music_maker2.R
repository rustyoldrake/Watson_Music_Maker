######################################################
### MUSIC MACHINE - CODE SNIPPET - IBM WATSON & R-AUDIO
### IBM Watson - Code Snippet ---  VERBAL INTERFACE FOR MUSIC
### Experimental Code. R Interface for IBM Watson Services
#####################################################

## Load our PACKAGES
library(audio)  # Audio Interface
library(gtools)
library(dplyr)
library(png)  # to place the graphic in the plots

####################### Housekeeping 
setwd("/Users/ryan/Documents/Project_R_Command_Control")  # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. # needs to be in your directory. 
source("music_notes.r")
source("music_maker_voice_command.r")

img <- readPNG("music_machine.png")
grid::grid.raster(img)

## Global Standard Variables
sample_rate <- 44100
notes <<- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)  # scale.  sharp are odd integers.
tempo <<- 120  # tempo <- 60 # slooower :)
duration <<- is.double(1)


# ORGANIC GENERATION # http://stackoverflow.com/questions/31782580/how-can-i-play-birthday-music-using-r
pitch <<- "D D E D G F#"  # you can also import a CSV file here.
duration <<- c(0.75, 0.25, 1, 1, 1, 2) # for some reason if all integers, later code dislikes
music_sheet <<- data_frame(pitch = strsplit(pitch, " ")[[1]], duration = duration)
music_sheet # let's LOOK! simple table - two columns - note and double that is duration 

## GENERATE FREQUENCIES
music_frequency_generate <- function(){
the_audio <<- music_sheet %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
        {suppressWarnings(as.numeric(.))} %>%
          ifelse(is.na(.), 4, .),
        note = notes[substr(pitch, 1, 1)],
        note = note + grepl("#", pitch) -
          grepl("b", pitch) + octave * 12 +
          12 * (note < 3), freq = 2 ^ ((note - 60) / 12) * 440)
}

## FUNCTION: Make Sine Wave    
make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) * freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade)) }

## FUNCTION: LOWER OCTAVE
music_lower_octave <- function(){
the_audio$freq <<- the_audio$freq / 2 } ## OCTAVE / PITCH ADJUST WITHOUT TIME IMPACT - FREQUENCY SHIFT 

## FUNCTION: HIGHER OCTAVE
music_higher_octave <- function(){
the_audio$freq <<- the_audio$freq * 2 } ## OCTAVE / PITCH ADJUST WITHOUT TIME IMPACT - FREQUENCY SHIFT 

## FUNCTION: FASTER TEMPO
music_faster_tempo <- function(){
  tempo <<- tempo * 1.5  } ## pick up the pace

## FUNCTION: SLOWER TEMPO
music_slower_tempo <- function(){
  tempo <<- tempo / 1.5  } ## pick up the pace


## FUNCTION DECLARE - GENERATE WAVE FOR PLAYBACK
music_wave_generate <- function(){
    #the_audio_wave <- mapply(make_sine, the_audio$freq, the_audio$duration) %>% do.call("c", .)
    return(mapply(make_sine, the_audio$freq, the_audio$duration) %>% do.call("c", .)) }



## Test Generation of Frequencies
music_sheet
music_frequency_generate() # takes what is in MUSIC SHEET music_sheet table and makes the_audio table
the_audio # let's LOOK - fancy table - two steps per note # SOURCE: # http://stackoverflow.com/questions/31782580/how-can-i-play-birthday-music-using-r

## Test generation of WAV from the audio
music_wave_data <- music_wave_generate() # REGENERATE music_wave_data WAV - process data and return WAV data
play(music_wave_data) # play WAV DATA


## LET"S PLAY!  Faster and higher
music_higher_octave()  # shift all notes higher scale
music_faster_tempo()   # speed it up! tempo ++
music_wave_data <- music_wave_generate() # REGENERATE music_wave_data WAV - process data and return WAV data
print(paste("audio table: ")); the_audio ; print(paste("tempo: ",tempo))
play(music_wave_data) # play WAV DATA


## LET"S PLAY!  Lower and slower
music_lower_octave()   # shift all notes lower scale
music_slower_tempo()   # slow it down tempo --
music_wave_data <- music_wave_generate() # REGENERATE music_wave_data WAV - process data and return WAV data
print(paste("audio table: ")); the_audio ; print(paste("tempo: ",tempo))
play(music_wave_data) # play WAV DATA


#### PART 2 - Nuts and bolts
pitch <<- "F C F C"  # you can also import a CSV file here.
duration <<- c(0.999, 1, 1, 1) # for some reason if all integers, later code dislikes
music_sheet <<- data_frame(pitch = strsplit(pitch, " ")[[1]], duration = duration)
music_frequency_generate() # takes what is in MUSIC SHEET music_sheet table and makes the_audio table
the_audio
music_wave_data <- music_wave_generate() # REGENERATE music_wave_data WAV - process data and return WAV data
play(music_wave_data) # play WAV DATA




#### PART 3 - let's overlay
pitch <<- "F C F C F C F C"  # eight beats
duration <- c(0.999, 1, 1, 1,1,1,1,1) # for some reason if all integers, later code dislikes
music_sheet <<- data_frame(pitch = strsplit(pitch, " ")[[1]], duration = duration)
music_sheet
music_higher_octave()  # shift all notes lower
music_lower_octave()  # shift all notes lower scale
music_frequency_generate() # takes what is in MUSIC SHEET music_sheet table and makes the_audio table
music_wave_data <- music_wave_generate() # REGENERATE music_wave_data WAV - process data and return WAV data

play(music_wave_data) # play WAV DATA


################# PART 4 - audio beatbox 4 - Multitone
audio.beatbox4 <- function(audio_tone1,audio_tone2,audio_period,audio_loops)
{
  while(audio_loops>0){  
    play(sin(1:audio_tone1),audio_tone1)
    play(sin(1:audio_tone2),audio_tone2)
    wait(audio_period)
    audio_loops = audio_loops - 1
  }
}

## Test
audio.beatbox4(C6,C8,.1,1) # harmony
play(sin(1:2000),697) # super manual

## FUNCTION DECLARE - Jamming on one ONE (record and playback)
############ USE THIS TO RECORD SAMPLES !!! ####################### AUDIO SAMPLE RATE
# https://www.youtube.com/watch?v=gbe0J1_g2sU - "Jamming on the one" 
jamming_on_the_one <- function(audio_period,audio_loops){
    sample_count <- 32000  ## samples
    sample_rate <- 16000 ## at 16khz
    wait(play(sin(1:1000),12000))  # 
    a <- record(sample_count, sample_rate, 2)
    wait(a) # wait for the recording to finish
    wait(play(sin(1:200),10000))  # 
    x <- a$data # get the result
    close(a); rm(a) # you can close the instance at this point
    audio <- x * 2 #   # amplify and crop the signal
    audio[audio < -1] <- -1
    while(audio_loops>0){  
      #play(sin(1:500),audio_tone1)
      #wait(audio_period)
      play(audio)
      wait(audio_period)
      audio_loops = audio_loops - 1
      }
}


### TEST OK LETS PLAY AROUND
jamming_on_the_one(2,4) # 1 second, 4 loops # (records sample and plays back for # cycles)
audio.beatbox4(C6,C7,.25,1) #audio_tone1,audio_tone2,audio_period,audio_loops)
audio.beatbox4(C7,C8,.25,1) #audio_tone1,audio_tone2,audio_period,audio_loops)








###### ###### ###### ###### ###### ###### ###### ###### 
#  let's do a really rough test
###### ###### ###### ###### ###### ###### ###### ###### 

## Initialize 
loops <- 2 # default
period <- .5 # default
low_tone <- C4
high_tone <- C5
mix_tone1 <- E6
mix_tone2 <- G4

## Loop  (later use #intents and #conversation on IBM Watson)
repeat
  {
    response <- watson.speech_to_text.sessionless("test.wav")
    print(response)
    
    # HELLO!  Greetings
    if(grepl("hello", response)){system_speaks("Hello, My name is MacKenzie! 
                                               I like music.
                                               Let's lay down some sick beats. OK?")}
    
    # OVERVIEW OF INSTRUCTIONS
    if(grepl("overview", response) || grepl("instructions", response)){system_speaks("Instructions for music maker.   
                            Tell me what kind of beat you want to put down, or say re-cord and loop the playback.
                            Key words include: sample, re-cord, base, regular, alternating, high, low, mixed and status.
                            You can also ask for status of key variables - or verbally set variables to new settings. ")}
    
    # STATUS OF KEY VARIABLES
    if(grepl("variables", response) || grepl("status", response)){
                            status_report <- paste(
                              "system status of music maker: ",",",
                              "low tone ", round(low_tone)," hertz. ",
                              "high tone ", round(high_tone)," hertz. ",
                              "mix tone 1 ", round(mix_tone1)," hertz. ",
                              "mix tone 2 ", round(mix_tone2)," hertz. ",
                              "number of loops ", loops,",",". ",
                              "beat period ", period, " seconds. ",
                              sep = ""
                            )
                            print(status_report)
                            system_speaks(status_report)}
                            # later do this by 'note name' (e.g. C or G)
     
    ## REGULAR BASE BEAT
    if(grepl("base", response) || grepl("bass", response) ||  grepl("regular", response)){
      audio.beatbox3(low_tone,low_tone,low_tone,low_tone,period,loops)  # test # tone 1,2,3,4, / audio_period /  loops
    }
    
    # INCREASE THE TONE TO HIGHER FREQUENCY
    # can also add "AND" for "OCATAVE or SCALE"  - doubling and halfing frequency is scale shift
    if(grepl("increase", response) || grepl("higher", response)){
      low_tone <- low_tone * 2
      high_tone <- high_tone * 2
      system_speaks(paste("Tone Scale Adjusted Up ",
                          "low tone ", round(low_tone)," hertz. ",
                          "high tone ", round(high_tone)," hertz. "))
      print(low_tone)
    }
    
    # DECREASE THE TONE TO LOWER FREQUENCY
    if(grepl("decrease", response) || grepl("lower", response)){
      low_tone <- low_tone / 2
      high_tone <- high_tone / 2
      system_speaks(paste("Tone Scale Adjusted Down ",
                          "low tone ", round(low_tone)," hertz. ",
                          "high tone ", round(high_tone)," hertz. "))
    }
    
    # FASTER PERIOD TEMPO
    if(grepl("faster", response) || grepl("fast", response) || grepl("allegro", response)){
      period <- period / 2
      system_speaks(paste("Tempo Adjusted Up ",
                          "beat period ", period, " seconds. "))
    }
    
    # SLOWER PERIOD TEMPO
    if(grepl("slower", response) || grepl("slow", response) || grepl("lento", response)){
      period <- period * 2
      system_speaks(paste("Tempo Adjusted Down ",
                          "beat period ", period, " seconds. "))
    }
    
    
    # RECORD A SAMPLE & PLAYBACK
    if(grepl("record", response) || grepl("sample", response)){
      #system_speaks("Track recording")
      jamming_on_the_one(period,loops)
    }
    

    # ALTERNATING BEAT
    if(grepl("alternating", response)){
      audio.beatbox3(low_tone,high_tone,low_tone,high_tone,period,loops)  # test # tone 1,2,3,4, / audio_period /  loops
      }
    
    # MIXED BEAT
    if(grepl("mixed", response) || grepl("overlapping", response)){
      audio.beatbox4(mix_tone1,mix_tone2,period,loops) #audio_tone1,audio_tone2,audio_period,audio_loops)
    }
    
    # MORE CYCLES (REPEATS)
    if(grepl("more", response) && grepl("cycles", response)){
      loops <- loops * 2
      system_speaks(paste("Cycles doubled to  ", loops, " loops "))
    }
    
    # LESS CYCLES (REPEATS)
    if(grepl("less", response) && grepl("cycles", response)){
      loops <- loops / 2
      system_speaks(paste("Cycles reduced to  ", loops, " loops "))
    }
    
  }





##########



# http://stackoverflow.com/questions/31782580/how-can-i-play-birthday-music-using-r

library("dplyr")
library("audio")
notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
pitch <- "D D E D G F# D D E D A G D D D5 B G F# E C5 C5 B G A G"
duration <- c(rep(c(0.75, 0.25, 1, 1, 1, 2), 2),
              0.75, 0.25, 1, 1, 1, 1, 1, 0.75, 0.25, 1, 1, 1, 2)
the_audio <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                   duration = duration)

the_audio <-
  the_audio %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
  {suppressWarnings(as.numeric(.))} %>%
    ifelse(is.na(.), 4, .),
  note = notes[substr(pitch, 1, 1)],
  note = note + grepl("#", pitch) -
    grepl("b", pitch) + octave * 12 +
    12 * (note < 3),
  freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 240
sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

the_audio_wave <-
  mapply(make_sine, the_audio$freq, the_audio$duration) %>%
  do.call("c", .)

play(the_audio_wave)

###  A BETTER WAY!
###  A BETTER WAY!
###  A BETTER WAY!

# http://stackoverflow.com/questions/31782580/how-can-i-play-birthday-music-using-r

library("dplyr")
library("audio")
notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
pitch <- "G A G A G A G"
duration <- c(0.999, 1, 1, 1, 1, 1, 1) # for some reason if all integers, later code dislikes
the_audio <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                   duration = duration)

the_audio <-
  the_audio %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
  {suppressWarnings(as.numeric(.))} %>%
    ifelse(is.na(.), 4, .),
  note = notes[substr(pitch, 1, 1)],
  note = note + grepl("#", pitch) -
    grepl("b", pitch) + octave * 12 +
    12 * (note < 3),
  freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 120
sample_rate <- 44100

## higher range - same time
# tempo <- 60
# sample_rate <- 22050  # lower rate is higher pitch - faster play (less quality too)
# 

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

## this is for OCTAVE / PITCH ADJUST WITHOUT TIME IMPACT
the_audio$freq <- the_audio$freq * 2

the_audio_wave <-
  mapply(make_sine, the_audio$freq, the_audio$duration) %>%
  do.call("c", .)

play(the_audio_wave)



