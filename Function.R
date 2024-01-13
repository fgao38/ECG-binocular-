#Function:Hello_World Function 
#Description: This function will return personalized greetings based on the weekdays, time, and mood.
#Parameters:weekday(from Monday to Sunday),time, and mood.
#Example: hello_world(weekday,"11","happy")
#Returns: A list of greetings from Monday through Sunday.

weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

hello_world <- function(weekday,time, mood){
  for (i in weekday){
  if ((i == "Tuesday" |
      i == "Thursday") &
      (time %in% c("11", "12")) & 
      (mood == "happy")){
    print(paste("Today is:", i, "Hello Dr. Dowling! I hope you are having a great day!"))}
  else if ((i == "Tuesday" |
           i == "Thursday") &
           (time %in% c("11", "12")) & 
           (mood == "sad")){
      print(paste("Today is:", i,"I am not feeling well"))}
  else if ((i == "Tuesday" |
           i == "Thursday") &
        (time %in% c("1", "2"))){
      print(paste("Today is:", i,"Good afternoon Dr.Hamilton"))}
  else if ((i == "Tuesday" |
           i == "Thursday") &
           (time %in% c("3", "4"))){
      print(paste("Today is:", i,"Good afternoon Dr.Wang"))}
  else{print(paste("Today is:", i,"Yes! I have no classes today"))}
}}

hello_world(weekday, "11", "happy")
hello_world(weekday, "1", "sad")
hello_world(weekday, "3", "happy")
hello_world(weekday, "7", "sad")



