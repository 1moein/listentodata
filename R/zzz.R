runtimequote <- function(){

  text = c("There is only one thing that makes a dream\n impossible to achieve: the fear of failure. - Paulo Coelho",
           "When I was 5 years old, my mother always \ntold me that happiness was the key to life. \nWhen I went to school, they asked me what\n I wanted to be when I grew up.\n I wrote down 'happy'. They told me I didn't\n understand the assignment, and I told them\n they didn't understand life. -John Lennon",
           "Life is like a camera. Just focus on what's\n important, capture the good times, develop from the \nnegatives, and if things don't work out,\n just take another shot. -Richard Branson",
           "When in doubt about an issue, \nshoot me an email for quick help :) -Moein Khanlari",
           "The secret of life, though, is to fall seven\n times and to get up eight times. -Paulo Coelho",
           "Don't judge each day by the harvest you reap\n but by the seeds that you plant. -Robert Louis Stevenson",
           "Why fit in when you were born to stand out? -Dr. Seuss",
           "Something not working right in the code? \nSend me an email for a quick hint :) -Moein Khanlari",
           "If you can't fly then run, if you can't\n run then walk, if you can't walk then crawl,\n but whatever you do you have to \nkeep moving forward. -Martin Luther King Jr.",
           "It is during our darkest moments that we\n must focus to see the light. -Aristotle",
           "Happiness is when what you think, what you say,\n and what you do are in harmony. -Mahatma Gandhi",
           "The secret of getting ahead is getting started. -Mark Twain",
           "If you get frustrated with a problem, \nshoot me an email for a quick hint :) -Moein Khanlari",
           "Life is 10% what happens to us and 90% how we react to it. -Charles R. Swindoll",
           "The best way to predict your future is to create it. -Abraham Lincoln",
           "In the end, we will remember not the words of our enemies, but the silence of our friends. -Martin Luther King Jr.",
           "The greatest glory in living lies not in never falling, but in rising every time we fall. -Nelson Mandela",
           "Success is not final, failure is not fatal: It is the courage to continue that counts. -Winston Churchill")
  todaysquote = paste("  ",text[sample(1:10, 1, replace = TRUE)])
  return(todaysquote)
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("                  ")
  packageStartupMessage(" Welcome to listentodata v.1.7 - Fall 2023")
  packageStartupMessage(" An R Package for Marketing Analytics by: Dr. M.K. ")
  packageStartupMessage(" Last updated: 11/21/2023 ")
  packageStartupMessage("                  ")
  packageStartupMessage("------------- Random Inspirational Quote------------")
  packageStartupMessage(runtimequote())
  packageStartupMessage("---------------------------------------------------")
  packageStartupMessage("                  ")

}

