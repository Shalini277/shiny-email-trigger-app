library(mailR)

send_notification_email <- function(user,error_msg){
  
  paste0('<!DOCTYPE html>
<html>
   <head>
     <style>
        body {
           margin-left: 20px;
           background-color: #1b1b1b;
	        background-repeat: no-repeat;
           background-attachment: fixed;
           background-position: center;
           text-align:center;
           }
         .header {
            background: url( "https://wind-public-files.s3.ap-south-1.amazonaws.com/wind-email/wplogo.jpg" ) no-repeat;
            height:100px;
            width:200;
            margin:-10px;
            background-color:#361755;
            }
         h1 {
            font-family: "Roboto","Helvetica Neue",Helvetica,Arial,sans-serif;
            font-size: 30px;
            font-style: normal;
            font-variant: normal;
            font-weight: bold;
            line-height: normal;
            color: black;
            
            }
         h2 {
            font-family: "Roboto","Helvetica Neue",Helvetica,Arial,sans-serif;
            font-size: 20px;
            font-style: normal;
            font-variant: normal;
            font-weight: bold;
            line-height: normal;
            color: grey;
            }
         p {
            margin-left: -20px;
            font-family: "Roboto","Helvetica Neue",Helvetica,Arial,sans-serif;
            font-size: 15px;
            font-style: normal;
            font-variant: normal;
            font-weight: normal;
            line-height: normal;
            color: grey;
            }
         a.button {
            color: black;
            background-color: #ffbf00;
            padding: 16px 32px;
            text-align: center;
            text-decoration: none;
            display: inline-block;
            font-size: 16px;
            margin: 4px 2px;
            transition-duration: 0.4s;
            cursor: pointer;
           }
        
     </style>
	 
   </head>
<body>
    <div style="background-color:#361755;height:100px;width:200;margin:-10px;">
    </div>
    <h1> ',error_msg,' CODE ERROR</h1>
    <h2> Hi ',user,',</h2>
    <h2> Please send request to us via email (shalini@sirpi.io).<br> We will solve the issue.</h2>
</body>
</html>
')  -> htmlmail
  
  
  send.mail(
    from=Sys.getenv('EMAIL_USERNAME'),
    to = user,
    cc = c(""),
    html = T,
    subject = "Error message",
    body = htmlmail,
    smtp = list(host.name = Sys.getenv('EMAIL_HOST'),
                port = 587,
                user.name = Sys.getenv('EMAIL_USERNAME'),
                passwd = Sys.getenv('EMAIL_PASSWORD'), 
                # ssl = T),
                tls = T),
    authenticate = T,  send = T
  )
  
}







  