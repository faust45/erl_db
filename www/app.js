window.onload = function() {
    var xhr = createRequestObject();
    //xhr.upload.onprogress  = onprogress;
    xhr.onreadystatechange = onreadystatechange;

    run();

    function run() {
        var url = "/"; 
        var formData = new FormData();
        formData.append("uploadfile", "data");

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

