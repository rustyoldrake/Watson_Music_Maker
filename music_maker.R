######################################################
### MUSIC MACHINE - CODE SNIPPET - IBM WATSON & R-AUDIO
### IBM Watson - Code Snippet ---  VERBAL INTERFACE FOR MUSIC
### Experimental Code. R Interface for IBM Watson Services
#####################################################

## Load our PACKAGES
library(audio)  # Audio Interface
library(gtools)


####################### Housekeeping 
setwd("/Users/ryan/Documents/Project_R_Command_Control")  # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. # needs to be in your directory. 

#img <- readPNG("ibm_watson.png")
img <- readPNG("music_machine.png")
grid::grid.raster(img)


# METHOD 1 - LOOKUP
notes_table <- read.csv("notes_table.csv",header=TRUE)
row.names(notes_table) <- notes_table$X
notes_table$X <- NULL
colnames(notes_table) <- 0:8
notes_table

# METHOD 2 - HARDWIRE
#Note		Frequency (Hz)
C0	<-	16.35
D0	<-	18.35
E0	<-	20.6
F0	<-	21.83
G0	<-	24.5
A0	<-	27.5
B0	<-	30.87
C1	<-	32.7
D1	<-	36.71
E1	<-	41.2
F1	<-	43.65
G1	<-	49
A1	<-	55
B1	<-	61.74
C2	<-	65.41
D2	<-	73.42
E2	<-	82.41
F2	<-	87.31
G2	<-	98
A2	<-	110
B2	<-	123.47
C3	<-	130.81
D3	<-	146.83
E3	<-	164.81
F3	<-	174.61
G3	<-	196
A3	<-	220
B3	<-	246.94
C4	<-	261.63
D4	<-	293.66
E4	<-	329.63
F4	<-	349.23
G4	<-	392
A4	<-	440
B4	<-	493.88
C5	<-	523.25
D5	<-	587.33
E5	<-	659.25
F5	<-	698.46
G5	<-	783.99
A5	<-	880
B5	<-	987.77
C6	<-	1046.5
D6	<-	1174.66
E6	<-	1318.51
F6	<-	1396.91
G6	<-	1567.98
A6	<-	1760
B6	<-	1975.53
C7	<-	2093
D7	<-	2349.32
E7	<-	2637.02
F7	<-	2793.83
G7	<-	3135.96
A7	<-	3520
B7	<-	3951.07
C8	<-	4186.01
D8	<-	4698.63
E8	<-	5274.04
F8	<-	5587.65
G8	<-	6271.93
A8	<-	7040
B8	<-	7902.13


wait(play(sin(1:2000/05),6000))  # 
wait(play(sin(1:2000/02),6000))  # 
wait(play(sin(1:2000/05),4000))  # 
wait(play(sin(1:2000/02),4000))  # 

wait(play(sin(1:2000/10)))  # 
wait(play(sin(1:2000/15)))  # 
wait(play(sin(1:2000/20)))  # 

wait(play( (sin(1:2000/10)+cos(1:2000/10)),8000))  # 

### basic example 1 - beatbox
for (i in 1:10){
  wait(play( (sin(1:2000/20)),10000))  # 
  Sys.sleep(.5)
  wait(play( (sin(1:2000/20)),6000)) 
  Sys.sleep(.5)
}

### basic example 2 - beatbox with airplane ping
for (i in 1:10){
  wait(play( (sin(1:2000/20)),10000))  # 
  Sys.sleep(.5)
  #wait(play( (sin(1:2000/20)+cos(1:2000/20)),8000))  # 
  #wait(play(audio_airping)) 
  wait(play( (sin(1:2000/20)),6000)) 
  Sys.sleep(.25)
  if (even(i)){
    print(i)
    #wait(play( (sin(1:2000/20)),6000)) 
    wait(play(audio_airping)) 
    #wait(play(audio_airping)) 
    Sys.sleep(.05)
    }
  else{Sys.sleep(.25)}
}
 

### BEATS - example 3 - hardcoded signnals
# 
# audio_duration <- 1000  # play 1:audio duration (2000 is about quarter secongd)
# audio_pause <- 0.5  # system sleep
# audio_tone1 <- 10000
# audio_tone2 <- 8000
# audio_tone3 <- 10000
# audio_tone4 <- 6000
# audio_loops <- 4
# 
# ### basic example 3 - beatbox quartern toes
# for (i in 1:audio_loops){
x <-   sin(1:audio_duration/20)
wait(play( x ,audio_tone1))  # 
plot()

#   Sys.sleep(audio_pause)
#   wait(play( (sin(1:audio_duration/20)),audio_tone2)) 
#   Sys.sleep(audio_pause)
#   wait(play( (sin(1:audio_duration/20)),audio_tone3))  # 
#   Sys.sleep(audio_pause)
#   wait(play( (sin(1:audio_duration/20)),audio_tone4)) 
#   Sys.sleep(audio_pause)
# }

wait(play(sin(1:2000/02),4000))  # 

### FUNCTION DECLARE - AUDIO BEATBOX1

audio.beatbox1 <- function(audio_tone1,audio_tone2,audio_tone3,audio_tone4,audio_pause,audio_loops)
{
  for (i in 1:audio_loops){
    wait(play( (sin(1:(audio_tone1/10))),audio_tone1))  # 
    Sys.sleep(audio_pause)
    wait(play( (sin(1:(audio_tone2/10))),audio_tone2)) 
    Sys.sleep(audio_pause)
    wait(play( (sin(1:(audio_tone3/10))),audio_tone3))  # 
    Sys.sleep(audio_pause)
    wait(play( (sin(1:(audio_tone4/10))),audio_tone4)) 
    Sys.sleep(audio_pause)
  }
}


### Test - C-B-A-G-F-E-D-C (C7 to C6) - DOWN SCALE
audio.beatbox1(2093,1975,1760,1567,0.25,1)  # test # duration / div / pause / tone 1,2,3,4, / loops
audio.beatbox1(1396,1318,1174,1046,0.25,1)  # test # duration / div / pause / tone 1,2,3,4, / loops

### Test - C-B-A-G-F-E-D-C (C6 to C5) - DOWN SCALE
audio.beatbox1(1046,987,880,783,0.25,1)  # test # duration / div / pause / tone 1,2,3,4, / loops
audio.beatbox1(698,659,587,523,0.25,1)  # test # duration / div / pause / tone 1,2,3,4, / loops

### Test - C-B-A-G-F-E-D-C (C5 to C4) - DOWN SCALE
audio.beatbox1(523,493,440,392,0.25,1)  # test # duration / div / pause / tone 1,2,3,4, / loops
audio.beatbox1(349,329,293,261,0.25,1)  # test # duration / div / pause / tone 1,2,3,4, / loops


audio.beatbox1(523,50,523,50,0.25,4)  # test # duration / div / pause / tone 1,2,3,4, / loops

play(sin(1:(880)),880)
play(sin(1:(440)),440)

play(sin(1:(100)),523)

################# audio beatbox 3

## Let's do four quarter seconds
while(1){  
  
  print("one")
  play(sin(1:(100)),523)
  wait(.5)
  
  print("two")
  play(sin(1:(100)),349)
  wait(.5)
  
  print("three")
  play(sin(1:(100)),523)
  wait(.5)
  
  print("four")
  play(sin(1:(100)),349)
  wait(.5)
  }


################# audio beatbox 3

audio.beatbox3 <- function(audio_tone1,audio_tone2,audio_tone3,audio_tone4,audio_period,audio_loops)
{
  while(audio_loops>0){  
    print("one")
    play(sin(1:100),audio_tone1)
    wait(audio_period)
    
    print("two")
    play(sin(1:100),audio_tone2)
    wait(audio_period)
    
    print("three")
    play(sin(1:100),audio_tone3)
    wait(audio_period)
    
    print("four")
    play(sin(1:100),audio_tone4)
    wait(audio_period)
    
    audio_loops = audio_loops - 1
    print(paste("loops remaining:",audio_loops))
  }
}

# test
audio.beatbox3(1046,783,523,392,0.5,1)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(349,329,293,261,1,2)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(1046,40,523,40,1,2)  # test # tone 1,2,3,4, / audio_period /  loops (40 is silence)


audio.beatbox3(1046,783,523,392,0.5,2)  # test # tone 1,2,3,4, / audio_period /  loops

audio.beatbox3(349,329,293,261,0.2,2)  # test # tone 1,2,3,4, / audio_period /  loops


## play with TEMPO
audio.beatbox3(392,392,392,392,1,2)  # test # tone 1,2,3,4, / audio_period /  loops


## play with TEMPO - DROPPING NOTES - Mixed Tempo
audio.beatbox3(1046,783,523,392,0.25,2)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(1046,783,523,392,0.5,2)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(1046,783,523,392,0.75,2)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(1046,783,523,392,1,2)  # test # tone 1,2,3,4, / audio_period /  loops

## play with TEMPO - HI CHIRP
audio.beatbox3(392,392,392,392,.5,1)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(392,4186,392,4186,0.25,2)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(392,392,392,392,.5,1)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(392,4186,392,4186,0.25,2)  # test # tone 1,2,3,4, / audio_period /  loops


## play with TEMPO - LOW BEAT WEAVE
audio.beatbox3(392,392,392,392,.5,1)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(392,261,392,261,0.5,1)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(392,392,392,392,.5,1)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(392,261,392,261,0.5,1)  # test # tone 1,2,3,4, / audio_period /  loops

## play with TEMPO - LOW BEAT WEAVE
audio.beatbox3(C5,E5,C5,E5,.5,2)  # test # tone 1,2,3,4, / audio_period /  loops



################# audio beatbox 4 - Multitone
audio.beatbox4 <- function(audio_tone1,audio_tone2,audio_period,audio_loops)
{
  while(audio_loops>0){  
    play(sin(1:audio_tone1),audio_tone1)
    play(sin(1:audio_tone2),audio_tone2)
    wait(audio_period)
    audio_loops = audio_loops - 1
  }
}

audio.beatbox4(C6,E7,.1,1)
audio.beatbox4(C6,C8,.1,1) # harmony
audio.beatbox4(C6,E7,.1,1)
audio.beatbox4(C6,C8,.1,1)
audio.beatbox4(C9,E9,.1,1)


play(sin(1:2000),697)

play(sin(1:2000),1209)
play(sin(1:2000),1336)
play(sin(1:2000),1477)

audio.beatbox4(C8,E8,1,2)


audio.beatbox5 <- function(audio_tone1,audio_period,audio_loops)
{
  while(audio_loops>0){  
    #play(sin(1:500),audio_tone1)
    #wait(audio_period)
    play(audio)
    wait(audio_period)
    audio_loops = audio_loops - 1
  }
}



############ USE THIS TO RECORD SAMPLES !!!
####################### AUDIO SAMPLE RATE
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



### OK LETS PLAY AROUND

jamming_on_the_one(1,4) # 1 second, 4 loops # (records sample and plays back for # cycles)
audio.beatbox3(E5,C6,E5,C6,.5,2)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox3(392,392,392,392,.5,1)  # test # tone 1,2,3,4, / audio_period /  loops
audio.beatbox4(C6,C7,.25,1) #audio_tone1,audio_tone2,audio_period,audio_loops)
audio.beatbox5(C4,1,1)





##########


######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services - SPEECH INTERFACE
######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(audio) 
library(data.table)
library(dplyr)
library(reshape2)
library(Rtts)
library(splitstackshape)
library(seewave) # need to play wav back?
library(stringr)
library(splitstackshape)
library(tidyr)
library(XML)
library(png)

closeAllConnections()

setwd("/Users/ryan/Documents/Project_R_Command_Control") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. Seperate R file looks sort of like below

## Base URLs for IBM Watson APIs
base_url_STT <- "https://stream.watsonplatform.net/speech-to-text/api"
base_url_TTS <- "https://stream.watsonplatform.net/text-to-speech/api/v1/synthesize"

sample_count <- 40000  ## 0k samples
sample_rate <- 16000 ## at 16khz - is ~5 seconds recording time

########## FUNCTION DECLARATIONS  #### FUNCTION DECLARATIONS ###########

## STT FUNCTION - Record!  
watson.STT.record <- function(samp_count,samp_rate)
{
  # record 8000 samples at 8000Hz (1 sec), mono (1 channel)
  # record 64k samples at 16kHz (4 sec), mono (1 channel), stereo = 2
  a <- record(samp_count, samp_rate, 2)
  wait(a) # wait for the recording to finish
  x <- a$data # get the result
  x[1:10] # show first ten samples
  close(a); rm(a) # you can close the instance at this point
  # amplify and crop the signal
  audio <- x * 2
  audio[audio < -1] <- -1
  audio[audio > 1] <- 1
  return(audio)
}


#### STT FUNCTION TO TIDY UP the STT response - just export the TRANSCRIPT ONLY
stt_transcript_only <- function(raw) 
{
  data <- as.data.frame(strsplit(as.character(raw),"\\n"))
  data <- data[c(7), ] # for now, grab just what we want
  data <- paste(data) # kill levels, - fyi this nukes confidence % info (may want later)
  data <- gsub("  ","",data) # remove excessive whitespace  0 cannot use ALL [[punct]] here
  data <- gsub("\\\\","",data) # remove punct we dont like
  data <- gsub("\"","",data) # remove punct we dont like
  data <- gsub("transcript","",data) # remove excessive whitespace
  data <- gsub(":","",data) # remove excessive whitespace - later: Improve this tidy step. 
  return(data) 
}


###### STT FUNCTION - ANalyze AUDIO WAV file with IBM Watson Speech to Text service - SESSIONLESS
watson.speech_to_text.recognize <- function(audio_file)
{ return(POST(url=paste(base_url_STT,"/v1/recognize",sep=""),
              authenticate(username_STT,password_STT),
              add_headers("Content-Type"="audio/wav"),
              body = (file = upload_file(audio_file))  
))} #works # hope this helps you with syntax!
## this is SESSIONLESS MODE - https://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/apis/#!/speech-to-text/recognizeSessionless


##### STT FUNCTION FOR SPEECH TO TEXT - RETURNS TRANSCRIPT - SESSIONLESS
watson.speech_to_text.sessionless <- function(file_name)
{ 
  wait(play(sin(1:200/4)))  # recording START tone
  print("RECORDING ------------------ (beep) ")
  the_audio <- watson.STT.record(sample_count,sample_rate)
  wait(play(sin(1:200/2)))  # recording STOP tone
  print("Recording COMPLETE --------- (beep) ")
  #print("Saving WAV File")
  save.wave(the_audio,file_name) 
  print("Calling IBM Watson Speech To Text API")
  response <- watson.speech_to_text.recognize(file_name)
  return(stt_transcript_only(content(response,"text")))
} 

####### TTS Function to list voices
watson.TTS.listvoices <- function()
{
  voices <- GET(url=paste("https://stream.watsonplatform.net/text-to-speech/api/v1/voices"),authenticate(username_TTS,password_TTS))
  data <- content(voices,"text")
  data <- as.data.frame(strsplit(as.character(data),"name"))
  data <- data[-c(1:2), ] # remove dud first row
  data <- strsplit(as.character(data),",")
  data <- data.frame(matrix(data))
  colnames(data) <- "V1"
  data <- cSplit(data, 'V1', sep="\"", type.convert=FALSE)
  data <- data.frame(data$V1_04)
  data[,1]  <- gsub("\\\\","",data[,1] )
  return(data) }

watson.TTS.listvoices()


########  TTS FUNCTION --- TEXT TO SPEECH
watson.TTS.execute <- function(url1,text1,voice1,filename1)
{
  the_audio = CFILE(filename1, mode="wb") 
  curlPerform(url = paste(url1,"?text=",text1,"&voice=",voice1,sep=""),
              userpwd = username_password_TTS,
              httpheader=c(accept="audio/wav"),
              writedata = the_audio@ref)
  close(the_audio)
  system(paste("open",filename1,"-a vlc"))
}

## Function System
system_speaks <- function(voice_transcript)
{
  voice <- "en-US_AllisonVoice"
  the_url <- paste(base_url_TTS,"?text=",URLencode(voice_transcript),"&voice=",voice,sep="")
  the_audio = CFILE("system_talks.wav", mode="wb")  ## here we receive the audio back
  curlPerform(url = the_url,userpwd = username_password_TTS,httpheader=c(accept="audio/wav"),writedata = the_audio@ref)
  close(the_audio)
  system("open system_talks.wav -a vlc")  # Now - Let's listen 
  wait(5) ## this gives ECHO some time to use the WiFI and not fight for bandwidth
}
system_speaks("testing 1 2 3")

###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### ###### ###### ###### ###### ###### ###### 
closeAllConnections()
file_name <- "file_name"






###### ###### ###### ###### ###### ###### ###### ###### 
#  let's do a really rough test
###### ###### ###### ###### ###### ###### ###### ###### 

## Initialize 
loops <- 4 # default
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
