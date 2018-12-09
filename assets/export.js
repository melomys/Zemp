window.onload = function (ev) {

var exportButton = document.getElementById("Exportieren");
    exportButton.addEventListener("click",function (ev) {

        var canvas = document.getElementById("canvas");
        var pdf = new jsPDF('l');
        var data = canvas.toDataURL("image/png",1.0);



        console.log(data);
        pdf.addImage(data,'PNG',0,0,297,210);
       // pdf.save("test.pdf")
        //  pdf.text("entiue",10,10);
        //  pdf.save("test.pdf");

    });

};

