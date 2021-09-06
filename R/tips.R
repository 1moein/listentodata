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
    cat("  1. To do an analysis in this course, we mainly need to load data sets into R and use an appropriate function to analyze them.\n\n")
    cat("  2. Before and after loading a dataset or running a line, check out the Environment(top right panel).\n\n")
    cat("  3. To learn how a function works, type ? followed by function name in the Console and press Enter/Return to get help on it.\n For example, try this in the Console: ?easyart\n\n")
    cat("  4. Most functions need one or more arguments to work. An argument is a parameter value that is fed into the function.\n For example, try: easyart(n=3). Then, replace the number 3 with a new value to feed it into the function and see how the results change.\n\n")
    cat("  5. Providing the name of an argument in the function call makes your code easier to understand. For example, you can use: easyart(n=4)\n Here n is the name of the argument. However, you do not have to explicitly do that. For example, easyart(4) does the same thing as easyart(n=4)\n\n")
    cat("  6. When excluding the names of the argument in the function call, R will assign the values you provide in the order shown in the function help file.\n For example, the help for easyart shows:  \"easyart(n = 2, mycolor = \"deeppink\", m = 0)\".\n So, in this case, easyart(5,\"black\") will work but, easyart(\"black\",5) won't.\n However, easyart(mycolor=\"black\",n=5) will work despite the wrong order, because the function will know which value belongs to which argument.\n\n")
    cat("  7. The help for easyart shows:  \"easyart(n = 2, mycolor = \"deeppink\", m = 0)\".\n In this function, there are three arguments, and all three have default values (2,deeppink, 0).\n As a result, if we don't provide any arguments, the function will work with these default values.\n To test this, try: easyart() at the Console. \n\n")
    
}

