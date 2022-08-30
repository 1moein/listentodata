#' A Few Points about Functions
#' 
#' This function prints a few points about functions in the Console.
#' 
#' @export
#' 
#' @examples
#' tips()
#' 
tips <- function(){
    
    cat("\014")
    cat("\n\n    A few important tips to remember     \n")
    cat("-----------------------------------------------\n\n")
    cat("  1. To do an analysis in this course, we mainly\n")
    cat("     need to load data sets into R and use an \n")
    cat("     appropriate function to analyze them.\n\n")
    cat("  2. Before and after loading a dataset or running\n")
    cat("     a line, you can check out the Environment\n")
    cat("     (top right panel) to see what new variable or\n")
    cat("     object has been created.\n\n")
    cat("  3. To learn how a function works, type ? followed\n")
    cat("     a function name in the Console and press \n")
    cat("     Enter/Return to get help on it. Try ?easyart\n\n")
    cat("  4. Most functions need one or more arguments to \n")
    cat("     work. An argument is a parameter value that \n")
    cat("     is fed into the function. For example, type:\n")
    cat("     easyart(n=3) and run it. Then, replace the number\n")
    cat("     3 with a new value and run again to see the change.\n\n")
    cat("  5. Providing the name of an argument in the function \n")
    cat("     call makes your code easier to understand. For\n")
    cat("     example, you can use: easyart(n=4)\n")
    cat("     But easyart(4) does the exact same thing. \n\n")
    cat("  6. When excluding the names of the arguments in \n")
    cat("     function call, R will assign the values you \n")
    cat("     provide in the order shown in the function \n")
    cat("     so, easyart(5,\"black\") will work. \n")
    cat("     but, easyart(\"black\",5) won't.\n\n")
    cat("  7. The help for easyart shows:  \n ")
    cat("    \"easyart(n = 2, mycolor = \"deeppink\", m = 0)\".\n")
    cat("     In this function, there are three arguments, \n") 
    cat("     and all three have default values (2,deeppink, 0).\n")
    cat("     As a result, if we don't provide any arguments, the \n") 
    cat("     function will use with these default values.\n")
    cat("     To test this, try: easyart() at the Console. \n")
    
}

