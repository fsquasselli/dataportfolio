<?php
if ($_SERVER["REQUEST_METHOD"] == "POST") {
    // Get form data
    $name = $_POST['name'];
    $email = $_POST['email'];
    $message = $_POST['message'];
    
    // Validate form data
    // You can add more advanced validation here
    
    // Send email
    $to = "fsquasselli@gmail.com"; // Your email address
    $subject = "Message from your website";
    $body = "Name: $name\nEmail: $email\nMessage: $message";
    $headers = "From: $email";

    if (mail($to, $subject, $body, $headers)) {
        echo "Email sent successfully.";
    } else {
        echo "Error sending email.";
    }
}
?>