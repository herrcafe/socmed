# socmed
Social Media Tools: a set of R functions


<b>get_emails_text</b> The function scraps text files for email addresses within the R environment. 
<b>get_emails_webpage</b> The function retrieves the email addresses listed in webpages that include email hyperlinks (if email.href=T) or the full body of the document (if email.href=F). This function takes one or multiple vectors of URLs and returns a vector with the emails found in the document (if return.df=F) or a data frame with the original URL(s) followed by emails on that page separated by columns (if return.df=T). The default arguments are return.df=F and email.href=F.
<b>get_url_title.R<b> This function takes one or multiple vectors of URLs and returns the title of each webpage (if return.df=F) or a data frame with the original URL(s) followed by the webpage title (if return.df=T).

--------------

# Author: Marco T. Bastos
# Date: 2014-10-08
# Modified: 2015-06-12
# Description: Extracts all email addresses from webpages and text files
# Imports: RCurl, XML, stringr, httr
