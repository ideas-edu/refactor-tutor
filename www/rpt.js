/**
 * Web front-end for Refactoring Programmming Tutor
 */

/*jslint browser: true */

const Hint = {"expand":"expand", "alt":"alt" }
const ExState = {"started":"started", "busy":"busy", "done":"done" }

var state = 
    { 
        "code"      : "",        // exerciseid
        "prefix"    : "[]", 
        "term"      : "", 
        "context"   : {},
        "user"      : ["","",""] // [userid, sessid, ---] 
    };
    
var exList = [];
var currExId = null;
var editor = null;
var BASE_URL = "/cgi-bin/";
var DR_URL = `${BASE_URL}/rpt`;
var LOGIN_URL = `${BASE_URL}/login`;
var debug = false;
var loginRequired = null;
var hintlevel = -1;
var userid = "0000";
var currentExState = ExState.started;

function initRPT() {

    if (!retrieveUser())
    {
        return;
    }

    setMode();

    $("#submitr").click(diagnoseR);
    $("#gethint").click(hint);
    $("#gethints").click(hints);
    $("#gethinttree").click(allhints);
    $("#dostep").click(hint);
    $("#toggleDebug").click(toggleDebug);
    $("#loadex").click(function() {
        if (currentExState == ExState.done || currentExState == ExState.started) 
        {
            loadExercise();
        }
        else
        {
            $('#sureNewEx').modal('show'); 
        }
    });
    $('#exlist').change(exSelected)
    $('#continueNewEx').click(function() {
        $('#sureNewEx').modal('hide'); 
        loadExercise();
    });

    hideAllMsg();
    $("#debug").hide();

    loadExercises();
    
    editor = ace.edit("editor");
    //editor.setTheme("ace/theme/monokai");
    editor.getSession().setMode("ace/mode/java");
    document.getElementById('editor').style.fontSize='14px';
    editor.getSession().on('change', function() {
        currentExState = ExState.busy;
    });
    
}

//-----------------------------------------------------------------------------
// Login page

function getLoginRequired(callback) {
    $.ajax({
        url: LOGIN_URL,
        success: function(r) {
            try {
                loginRequired = JSON.parse(r);
                callback(loginRequired);
            } catch (f) {
                $("#err").html("Malformed response");
                $("#errbox").show();
                toConsole(f);
            }
        },
        timeout: 100000
     }).fail(
         function (f)
         {
            $("#err").html("Unknown error");
            $("#errbox").show();
            toConsole(f);
         } 
     );
}

function handleLogin()
{
    var id = $('#userid').val();
    if (id.length === 0 || !id.trim() || !isValidId(id.trim()))
    {
        $("#err").html('Please provide a valid id');
        $("#errbox").show();
        $('#userid').select();
    }
    else
    {
        login(id)
    }
}
function login(id) {
    localStorage.setItem("rpt:userid", id);
    window.location = ".";
}
function isValidId(id)
{
    if (id.length !== 4)
    {
        return false;
    }
    var n = parseInt(id);
    if (isNaN(n))
    {
        return false;
    }
    var check = n % 10;
    var m = div(n,10);
    var sum = ((m % 10) * 3) + ((div(m,10) % 10) * 7) + ((div(m, 100) % 10) * 3);
    return sum % 10 === check;
}
function div (n, m) { return Math.floor(n/m); }


//-----------------------------------------------------------------------------
// State utils

function stateToArray(state) {
   return [state.code
       , state.prefix
       , prepareCode(encodeURIComponent(state.term))
       , state.context
       , state.user
      ];
}

function saveState(arr) {
    state.code = arr[0];
    state.prefix = arr[1];
    state.term = arr[2]; //escape(arr[2].htmlToAscii());
    state.context = arr[3];
}

function emptyState()
{
    state.prefix = [];
    state.term = "";
    return state;
}

function resetState()
{
    state = 
    { 
        "code": "", 
        "prefix": "[]", 
        "term": "", 
        "context": {},
        "user"   : ["","",""]
    };
    state.user[0]  = userid;
}

//-----------------------------------------------------------------------------
// User actions

function setMode()
{
    if (!debug) 
    {
        $("#gethint").hide();
        $("#gethints").hide();
        $("#dostep").hide();
        $("#toggleDebug").hide();
    }
}
function toggleDebug()
{
    debug = ! debug;
    $("#debug").toggle();
}

function toDebug(s)
{
    if (debug)
    {
        var curr = $("#debug").val();
        $("#debug").val(s + "\n" + curr);
    }
}

function toConsole(s)
{
    if (debug)
    {
        console.log(s);
    }
}

function loadExercises()
{
     serviceCall(makeRequest("exerciselist", []), 
        function(res)
        {
            toConsole(res);
            exList = res.result;
            $("#exlist").find('option').remove();
            if (exList.length == 0)
            {
                $("#exlist").append("<option>No exercises</option>"); 
            }
            else
            {
                $(exList).each(function(i) { $("#exlist").append("<option>" +  exList[i].exerciseid + "</option>"); });
                loadExercise();
            }
        }
    );
}

function exSelected()
{
    var id = $('#exlist').find(":selected").text();
    if (id != currExId)
    {
        $("#loadex").html("Start exercise");
    }
}

function loadExercise()
{
    $("#loadex").html("Restart exercise");
    var id = $('#exlist').find(":selected").text();
    currExId = id;
    var exercise = findEx(currExId);

    hideAllMsg();

    if (! retrieveUser())
    {
        return;
    }
    $.get(DR_URL,
        'input=' + makeRequest("example", [id,0, userid]), 
        function(res)
        {
            if(res.result == null)
            {
                showMsg("Unable to load exercise.", msgtype.ERROR);
                return;
            }
            toConsole(res);
            saveState(res.result);
            editor.setValue(state.term);

            $("#exname").html("Exercise: " + currExId);
            $("#exdesc").html(toHTML(exercise.description));
            
            // prepare editor
            editor.getSession().setUndoManager(new ace.UndoManager()); // clear edit history
            editor.setValue(state.term);
            editor.gotoLine(1);
            $("#editor").focus();
          
            resetHintlevel();
            currentExState = ExState.started;
        }
    ).fail (function (x) {
        showMsg("Unable to load exercise.", msgtype.ERROR);
        editor.setValue("");
        resetState();
    });
}

function findEx(id)
{
    for (var i = 0; i < exList.length; i++)
        if (exList[i].exerciseid == id)
            return exList[i];
    return null;
}

function diagnoseR (ev)
{
    hideAllMsg();
    resetHintlevel();
    setWait(ev.currentTarget, true);
    currentExState = ExState.busy;

    var code = encodeURIComponent(editor.getValue()); 
    var encodedCode = prepareCode(code);
    //console.log(encodedCode);

    serviceCall(makeRequest("diagnoseR", [stateToArray(state), encodedCode]), 
        function(response)
        {
            setWait(ev.currentTarget, false);

            toDebug("diagnoseR response: \n" + JSON.stringify(response));
            if(response == null)
            {
                showMsg("Something went wrong..", msgtype.ERROR);
                return;
            }
            
            var res = response.result;
            if (res == null)
            {
                showMsg(response.error, msgtype.ERROR);
                return;
            }
            
            var diagnoseType = Object.keys(res)[0];  
            var ok = false;
            var exDone = false;     
            if (diagnoseType == "syntaxerror")
            {
                var msg = res[diagnoseType];
            }
            else if (diagnoseType == "notequiv")
            {
                var msg = res[diagnoseType][0].reason;
            }
            else if (diagnoseType == "buggy")
            {
                var msg = res[diagnoseType][1];
            }
            else if (diagnoseType == "similar" || diagnoseType == "expected" || diagnoseType == "correct") 
            {
                var msg = diag[diagnoseType] || "Unknown progress.";
                var newState = res[diagnoseType][1];
                toConsole("new state saved");
                toConsole(newState);
                saveState(newState);

                exDone = res[diagnoseType][0]["ready"];
                ok = true;
            }
            if (exDone) 
            {
                currentExState = ExState.done;
                showDone();
            }
            else 
            {
                showMsg(msg, diagToMsgType(diagnoseType));
                if (ok)
                {
                    stepsRemaining();
                }
            }
        }
    );
}
const diag = { similar:  "You haven't changed much in the code.", 
               expected: "That was a correct step, well done!", 
               correct:  "All test cases still pass." }

// variant voor IPT
function diagnoseTextDeep(ev)
{
    hideMsg();
    resetHintlevel();
    
    var code = encodeURIComponent(editor.getValue()); 
    var encodedCode = prepareCode(code);
    toConsole(encodedCode);
    serviceCall(makeRequest("feedbacktextdeep", [stateToArray(state), encodedCode]), //
        function(response)
        {
            toDebug("Diagnose text response: \n" + JSON.stringify(response));
            if(response == null)
                showMsg("Something went wrong..", msgtype.ERROR);
            
            var res = response.result;
            if (res == null)
                showMsg(response.error, msgtype.ERROR);
            else
            {
                var msg         = res[1];
                var newState    = res[2];
                var ready       = res[3];
                var diagnoseType = res[4];
                showMsg(msg, diagToMsgType(diagnoseType) );
                if (diagnoseType == "similar" || diagnoseType == "expected") 
                    saveState(newState);
                if (ready) alert ("You are done!");
            }
        }
    );
}

function hint(ev)
{  
    var doExecute = false;
    if (ev.target.id == "dostep")
        doExecute = true;
    
    serviceCall(makeRequest("onefirsttextR", [stateToArray(state)]), 
        function(response)
        {   
            toDebug("Hint response: \n" + JSON.stringify(response));
            var result = response.result;
            toConsole(result);
            if(result == null)
            {
                showMsg(response.error, msgtype.ERROR);
            }
            else
            {
                if (doExecute)
                {
                    saveState(result[1]);
                    editor.setValue(toX(state.term)); 
                }
                
                showMsg(result[0], msgtype.HINT);
            }
        }
    ); 
    stepsRemaining();
}

function hints(ev)
{    
    serviceCall(makeRequest("allfirststextR", [stateToArray(state)]), 
        function(response)
        {   
            toDebug("Hints response: \n" + JSON.stringify(response));
            var results = response.result;
            toConsole(results);
            if(results == null)
            {
                showMsg(response.error, msgtype.ERROR);
            }
            else
            {
                var res = "";
                for (i = 0; i < results.length;i++)
                {     
                     res += "<li>" + results[i][0] + "<br>";
                    //$('#msgs').append('<div class="alert" > <span id="feedbacktext"></span></div>');
                    
                }
                showMsg(res,  msgtype.HINT);
            }
        }
    ); 
}

// First do a diagnosis, then ask hints
function allhints(ev)
{ 
    hideAllMsg();
    setWait(ev.currentTarget, true);

    var code = encodeURIComponent(editor.getValue()); 
    var encodedCode = prepareCode(code);

    currentExState = ExState.busy;

    // first do a diagnose
    serviceCall(makeRequest("diagnoseR", [stateToArray(state), encodedCode]), 
        function(response)
        {
            toDebug("diagnoseR response: \n" + JSON.stringify(response));
            if(response == null)
            {
                showMsg("Something went wrong..", msgtype.ERROR);
                return;
            }
            
            var res = response.result;
            if (res == null)
            {
                showMsg(response.error, msgtype.ERROR);
                return;
            }
            
            var diagnoseType = Object.keys(res)[0]; 
            var ready = false;        
            var canAskForHints = false;  
            var msg = "";
            if (diagnoseType == "syntaxerror")
            {
                var msg = res[diagnoseType];
            }
            else if (diagnoseType == "notequiv")
            {
                var msg = res[diagnoseType][0].reason;
            }
            else if (diagnoseType == "buggy")
            {
                var msg = res[diagnoseType][1];
            }
            else if (diagnoseType == "similar" || diagnoseType == "expected" || diagnoseType == "correct") 
            {
                var newState = res[diagnoseType][1];
                toConsole("new state saved");
                toConsole(newState);
                saveState(newState);

                ready = res[diagnoseType][0]["ready"];
                canAskForHints = true;
            }
            setWait(ev.currentTarget, false);
            if (ready) 
            {
                showDone();
            }
            else if(canAskForHints)
            {
                getallhints(ev);
            }
            else 
            {
                showMsg(msg, diagToMsgType(diagnoseType));
            }
        }
    );

}

var hints;
function getallhints(ev)
{    
    setWait(ev.currentTarget, true);
    hideMsg();

    serviceCall(makeRequest("allhints", [stateToArray(state)]), 
        function(response)
        {   
            setWait(ev.currentTarget, false);

            if (response.result == null)
            {
                showMsg(response.error, msgtype.ERROR);
                return;
            }

            toDebug("Hints response: \n" + JSON.stringify(response));
            var tree = response.result;
            
            //tree[DESCRIPTION] = "";//<strong>Options:</strong>";
            numberTree(tree, 0);
            numberNextSib(tree, -1);
            var hintTree = createTree(tree);
            
            showMsg(hintTree,  msgtype.HINT);
            $("[id|='l']").hide(); // hide all  
            $("[id|='more-0']").hide();
            $("[id|='l-0']").show(); //show root           
            $("[id|='l-1']").show(); //show top level        
            
             // add handlers 
            $("[id|='more']").click(function more (e) {          
                hintAction(e, Hint.expand, tree, $(this))             
            });
            $("[id|='alt']").click(function alt(e) {
                hintAction(e, Hint.alt, tree)
            });
        }   
    );
}

function hintAction(e, type, tree,)
{
    var parts = e.currentTarget.id.split("-");
    var fromId = parseInt(parts[1]);
    var toShowId = fromId;
    if (type === Hint.expand)
    {
         toShowId = fromId + 1;
    }
    else if (type === Hint.alt)
    {
        toShowId = getNode(tree, fromId);
    }
    $('#l-' + toShowId).show();    
    $(e.currentTarget).hide();               

    reportHintAction(tree, fromId, toShowId, type);
}

function reportHintAction(tree, fromNr, toNr, type)
{
    //console.log('clicked on ' + fromNr + ', ' + type + ' to ' + toNr);

    var fromS = $('#t-' + fromNr).text();
    var toS = $('#t-' + toNr).text();
    
    serviceCall(makeRequest("expandHint", [stateToArray(state), fromNr, type, 
            prepareCode(encodeURIComponent(fromS)), prepareCode(encodeURIComponent(toS))]), // could be code
                    function(response) { } );
}

function stepsRemaining()
{
    serviceCall(makeRequest("hintsremaining", [stateToArray(state)]), 
        function(response)
        {   
            toDebug("Stepsremaining response: \n" + JSON.stringify(response));
            var steps = response.result;
            if (steps > 0)
            {
                $("#stepsleft").html('There are still some improvements left (' + steps + ')');
                $("#stepsleft").show();
            }
            else
            {
                $("#stepsleft").hide();
            }
        }
    ); 

}

//-----------------------------------------------------------------------------
// Tree

var BRANCHES = 2, PREFIXNR = 3, NEXTSIB = 4, DESCRIPTION = 0, HINTTYPE= 1;

// return next sibling
function getNode(tree, curnr)
{
    if (tree == {} || tree == null)
        return null;
        
    if ( tree.Tree[PREFIXNR] == curnr)
        return tree.Tree[NEXTSIB];
        
    var branches = tree.Tree[BRANCHES];
    for (var i = 0; i < branches.length; i++)
    {
        var res = getNode (branches[i], curnr);
        if(res != null)
            return res;
    } 
    return null;
}

function numberTree(tree, nr)
{
    if (tree == {} || tree == null)
        return -1;
  
    tree.Tree[PREFIXNR] = nr;     
    var branches = tree.Tree[BRANCHES];
    for (var i = 0; i < branches.length;i++)
    {
        nr = numberTree (branches[i], nr + 1);
    } 
    return nr;
}

function numberNextSib(tree, nr)
{
    if (tree == {} || tree == null)
        return ;
  
    tree.Tree[NEXTSIB] = nr;     
    var branches = tree.Tree[BRANCHES];
    for (var i = 0; i < branches.length;i++)
    {
        var nextSib = -1;
        if (i + 1 < branches.length) nextSib = branches[i + 1].Tree[PREFIXNR];
        numberNextSib (branches[i], nextSib);
    } 
}

function createTree (tree)
{
    if (tree == {} || tree == null)
        return "";
          
    tree = tree.Tree;
    var desc = tree[DESCRIPTION];
    var label = tree[PREFIXNR];
    var nextSib = tree[NEXTSIB];
    var branches = tree[BRANCHES];
    var hinttype = tree[HINTTYPE];
    
    var id = "l-" + label;
    var moreId = "more-" + label;
    var altId = "alt-" + label;
    var textId = "t-" + label;

    var descPart = "<span id ='" + textId + "'>" + desc + "</span>";

    var more = '<br><button type="button" class="btn btn-link btn-xs" id="' + moreId+ '">Explain more <i class="fa fa-plus-square" aria-hidden="true"></i></button>';      

    var alt = "";
    if (nextSib >= 0)
        alt = '<button type="button" class="btn btn-link btn-xs" id="' + altId+ '">Another hint <i class="fa fa-level-down" aria-hidden="true"></i></button>';       
    
    if (branches.length == 0 && hinttype === "step")
        return "<li id='" + id + "'>" +  "Try to use this example code: <pre class='card p-2' id ='" + textId + "'><code>" + desc + "</code></pre>" + alt;

    if (branches.length == 0)
        return "<li id='" + id + "'>" + descPart + alt;
    
    // there are children
    var res = "";
    for (var i = 0; i < branches.length;i++)
    {
        res += createTree (branches[i]);
    }  
    
    return "<li id='" + id + "'>" + descPart + more + alt + "</li>"  + "<ul>" + res + "</ul>";
}


//-----------------------------------------------------------------------------
// Util functions

function prepareCode(code)
{
   //var ecode = encodeURIComponent(code);
    return code.replace(/%0A/ig, "\n").replace(new RegExp("%22", "g"), "\""); // i=case insensitive, g=all matches
}

// newer?
function toService(code)
{
    var ecode = encodeURIComponent(code); 
    ecode = ecode.replace(new RegExp("%22", "g"), "\"");
    return ecode.replace(new RegExp("%0A", "g"), ""); // remove linebreaks
}

function toX(s)
{
    return s.replace(/\\n/g, "\n");
}

function toHTML(text)
{
    return text.replace(/\r\n/g, '<br />').replace(/[\r\n]/g, '<br />').replace (/\\n/g, "<br />");
}

function makeRequest(method, params)
{
    var request = { "source" : "rpt"
                 , "event"  : ""
                 , "method" : method
                 , "params" : params
                 , "id"     : "0"
                 };
    var s = JSON.stringify(request);
    toDebug("Request: \n" + s);
    return s;
}

var msgtype = {
    ERROR    : 0,
    HINT     : 1,
    SUCCESS  : 2,
    WARNING  : 3
    
}
var alertClasses = ["alert-danger", "alert-info", "alert-success", "alert-warning"];
var typeTexts = ["Error", "Hint", "Feedback", "Feedback"];

function diagToMsgType(diagString)
{
    if (diagString == "expected" || diagString == "similar")
        return msgtype.SUCCESS;
    if (diagString == "correct")
        return msgtype.SUCCESS;
    return msgtype.ERROR;
}

function showMsg(msg, type)
{
    $("#feedbackbox").show();
    $("#feedbacktext").html('<strong>' + typeTexts[type] + '</strong>: ' + msg);
    
    $("#feedbackbox").removeClass( "alert-danger alert-info alert-succes alert-warning" );
    $("#feedbackbox").addClass(alertClasses[type]);
}

function hideMsg()
{
    $("#feedbackbox").hide();
}

function hideAllMsg()
{
    $("#feedbackbox").hide();
    $("#done").hide();
    $("#stepsleft").hide();
}

function showDone()
{
    $("#done").show();
} 

function appendCode(code)
{
    var cur = editor.getValue(); 
    var col = editor.getCursorPosition().column;
    if (col == 0)
        editor.insert(code + "\n"); 
    else
        editor.insert("\n" + code + "\n"); 
}

function resetHintlevel() { hintlevel = -1; }

function setWait(button, waitOn)
{
    if ($(button).attr('disabled')) //waiting
    {
        if (!waitOn) //switch off
        {
            $(button).children().first().remove();
            $(button).attr('disabled', false).children().first().show();
        }
    }
    else if (waitOn)
    {
        $(button).attr('disabled', true).children().first().hide();
        $(button).prepend('<div class="spinner-border spinner-border-sm"></div>');
    }
}

function enableButtons()
{
    $(":button").each(function() { setWait(this, false); } );
}

//-----------------------------------------------------------------------------
// Service calls

function serviceCall(request, successCallback)
{
    // check again for valid userid
    if (!retrieveUser())
    {
        return;
    }
    $.ajax({
       url: DR_URL,
       data: 'input=' + request,
       success: successCallback,
       timeout: 100000
    }).fail(
        function (f)
        {
            showMsg("Unknown error", msgtype.ERROR);
            toConsole(f);
            enableButtons();
        } 
    );   
}

// Assumes getLoginRequired has been called before
function retrieveUser()
{
    var storedUserid = localStorage.getItem("rpt:userid");

    if(!loginRequired) {
        storedUserid = "0000";
    }

    if(!storedUserid || !isValidId(storedUserid.trim()) )
    {
        window.location="login.html";
        return false;
    }
    else
    {
        userid = storedUserid;
        state.user[0] = userid;
        $("#useridtext").html('User: ' + userid);
        return true;
    }
}

function isValidId(id)
{
    if (id.length !== 4)
    {
        return false;
    }
    var n = parseInt(id);
    if (isNaN(n))
    {
        return false;
    }
    var check = n % 10;
    var m = div(n,10);
    var sum = ((m % 10) * 3) + ((div(m,10) % 10) * 7) + ((div(m, 100) % 10) * 3);
    return sum % 10 === check;
}
function div (n, m) { return Math.floor(n/m); }
