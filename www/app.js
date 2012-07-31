window.onload = function() {
    var dropZone = document.getElementById('drop_zone');
    dropZone.addEventListener('drop', handleFileSelect, false);
    dropZone.addEventListener('dragover', handleDragOver, false);
    dropZone.addEventListener('dragleave', stopEvent, false);

    function handleFileSelect(e) {
        stopEvent(e); 
        send(e.dataTransfer.files); 
    }

    function handleDragOver(e) {
        stopEvent(e); 
    }

    function stopEvent(e) {
        e.stopPropagation();
        e.preventDefault();
    }

    function send(files) {
        var xhr = createRequestObject();
        //xhr.upload.onprogress  = onprogress;
        xhr.onreadystatechange = onreadystatechange;

        run();

        function run() {
            var url = "/"; 
            var formData = new FormData();
            formData.append("uploadfile", "data");
            formData.append("name", "faust45");
            formData.append("file", files[0]);

            xhr.open("POST", url, true);
            xhr.send(formData);
        }

        function onComplete() {
            var response;

            if (xhr.status == 201 || xhr.status == 200) {
                try { 
                    response = eval("(" + xhr.responseText + ")"); 
                }
                catch(err) { response = {}; }

                alert(response);
            }
        }

        function onreadystatechange() {
            if (xhr.readyState == 4) { onComplete(); }
        }
    }
}

function createRequestObject() {
    var request = null;

    if (!request) try {
        request = new ActiveXObject('Msxml2.XMLHTTP');
    } catch (e) {}
    if (!request) try {
        request = new ActiveXObject('Microsoft.XMLHTTP');
    } catch (e) {}
    if (!request) try {
        request = new XMLHttpRequest();
    } catch (e) {}

    return request;
}

