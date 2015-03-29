(function()
{
    _.each(categories
          , function(value, category, list)
          {
                $("#" + category + "-link").css(
                    { "background-color"    :   value["header"]["background-color"]
                    }
                );
                $("#" + category + "-link > a").css(
                    { "color"               :   value["header"]["text-color"]
                    , "size"                :   "2em"
                    , "font-weight"         :   "bold"
                    }
                );
                $("#" + category + "-link").hover(function(){
                    $(this).css(
                        { "background-color"    :   value["header"]["text-color"]
                        }
                    );
                });
                $("#" + category + "-link > a").hover(function(){
                    $(this).css(
                        { "color"               :   value["header"]["background-color"]
                        , "size"                :   "2em"
                        , "font-weight"         :   "bold"
                        }
                    );
                });

                console.log(value["header"]["text-color"]);
                console.log($("#" + category + "-link > a"));
          }

    )
}());
