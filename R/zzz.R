runtimequote <- function(){

  text = c("There is only one thing that makes a dream impossible to achieve: the fear of failure. - Paulo Coelho",
           "When I was 5 years old, my mother always told me that happiness was the key to life. When I went to school, they asked me what I wanted to be when I grew up. I wrote down 'happy'. They told me I didn't understand the assignment, and I told them they didn't understand life. -John Lennon",
           "Life is like a camera. Just focus on what's important, capture the good times, develop from the negatives, and if things don't work out, just take another shot. -Richard Branson",
           "The secret of life, though, is to fall seven times and to get up eight times. -Paulo Coelho",
           "Don't judge each day by the harvest you reap but by the seeds that you plant. -Robert Louis Stevenson",
           "Why fit in when you were born to stand out? -Dr. Seuss",
           "If you can't fly then run, if you can't run then walk, if you can't walk then crawl, but whatever you do you have to keep moving forward. -Martin Luther King Jr.",
           "It is during our darkest moments that we must focus to see the light. -Aristotle",
           "Happiness is when what you think, what you say, and what you do are in harmony. -Mahatma Gandhi",
           "The secret of getting ahead is getting started. -Mark Twain")
  todaysquote = paste("  ",text[sample(1:10, 1, replace = TRUE)])
  return(todaysquote)
}


.onLoad <- function(libname, pkgname) {
  packageStartupMessage("                  ")
  packageStartupMessage(" Welcome to Marketing Analytics v.1.0")
  packageStartupMessage(" by: Dr. MK for MKTG763 @ UNH-Paul College")
  packageStartupMessage("                  ")
  packageStartupMessage("                  ")
  packageStartupMessage("-----A Small Dose of Inspiration-----")
  packageStartupMessage("                  ")
  packageStartupMessage(runtimequote())
  packageStartupMessage("                  ")
  packageStartupMessage("-------------------------------------")
  packageStartupMessage("                  ")

}
