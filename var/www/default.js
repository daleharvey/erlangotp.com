ErlangOtp = (function() { 
    
    var public = {};

    var converter = new Showdown.converter();
    var text = null;
    initEvents();

    public.startEdit = function(content) { 

        $("#preview").attr("checked", false);
        public.unPreview();

        $("#content").fadeOut( "fast", function() { 
            $("#wiki").fadeIn("fast");
        });
        $("#wiki textarea").val(content);
    };

    public.cancelEdit = function() { 
        $("#wiki").fadeOut( "fast", function() { 
            $("#previewwrapper").hide();
            $("#content").fadeIn("fast");
        });
     };

    public.preview = function() { 
        var text = $("#wiki textarea").val();
        var html = converter.makeHtml(text);
        $("#editwrapper").fadeOut( "fast", function() {
            $("#previewwrapper").empty().append(html).fadeIn("fast");
        });
    };

    public.unPreview = function() { 
        $("#previewwrapper").fadeOut( "fast", function() {
            $("#editwrapper").fadeIn("fast");
        });
    };

    public.save = function() {
        
        var markdown = $("#wiki textarea").val();
        var html     = converter.makeHtml(markdown);
        
        $.ajax({
            "type"     : "POST",
            "dataType" : "json",
            "data"     : JSON.stringify({"html":html, "markdown":markdown}),
            "success"  : function(data) {

                $("#content").html(html);
                var notice = $("<div class='notice'>Your page was saved, "
                               +"yay</div>");
                               
                var hide = function() {                     
                    notice.fadeOut(function() {
                        notice.remove();
                    }) 
                };
                setTimeout( hide, 3000 );
                $("#content").before(notice);
                public.cancelEdit();
            }
        });
    };

    function docName() { 
        var path = document.location.pathname.replace(/\//g, "");
        return (path == "") ? "index" : path;
    };

    function initEvents() { 

        $(".page-name").text(docName());

        $("#preview").bind("change", function() { 
            if( $(this).is(":checked") ) { 
                public.preview();
            } else {
                public.unPreview();
            }
        });

        $(".cancel-edit").bind("mousedown", function() { 
            public.cancelEdit();
        }); 

        $(".wiki-create").bind("mousedown", function() { 
            public.startEdit("");
        }); 

        $(".save-page").bind("mousedown", function() { 
            public.save();
        }); 

        $(".edit-page").bind("mousedown", function() { 
            $.ajax({
                "url"     : document.location.pathname+"?markdown", 
                "cache"   : false,
                "success" : function(data) { 
                    public.startEdit(data);
                }
            });
        }); 

    };
})();