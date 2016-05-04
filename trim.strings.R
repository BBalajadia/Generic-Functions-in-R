#####################################################################
#     Bryan R. Balajadia                                           
#     Fn: trim.strings                                             
#     Function to trim leading and/or trailing whitespace in R     
#####################################################################

# Arguments:    x - character vector
#            side - side(s) on which to remove whitespace 
#                   default : "both"
#                   possible values: c("both", "leading", "trailing")

trim.strings <- function(x, side = "both") { 
	if (is.na(match(side, c("both", "leading", "trailing")))) { 
	  side <- "both" 
	  } 
	if (side == "leading") { 
	  sub("^\\s+", "", x)
	} else { 
    if (side == "trailing") { 
    sub("\\s+$", "", x)
    } else gsub("^\\s+|\\s+$", "", x)
	} 
} 

#--------------------------------------------------------------------
# Illustration

a <- c("   ABC123 456    ", " ABC123DEF          ")

# returns string without leading and trailing whitespace
trim.strings (a)
# [1] "ABC123 456" "ABC123DEF" 

# returns string without leading whitespace
trim.strings (a, side = "leading")
# [1] "ABC123 456    "      "ABC123DEF          "

# returns string without trailing whitespace
trim.strings (a, side = "trailing")
# [1] "   ABC123 456" " ABC123DEF"   

