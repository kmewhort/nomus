<html>
<body>
<form enctype="multipart/form-data" action="http://<?php echo $_SERVER['SERVER_NAME']; ?>:8090/docconverter/convert" method="POST">
Choose a file to upload: <input name="uploadedfile" type="file" /><br />
To type: <input type="text" name="to" />
<br />
<input type="submit" value="Upload File" />
</form>
</body>
</html>
