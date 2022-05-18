//
// JavaScript Common functions
// Copyright (c) 2006 e-Xan
//
function showMessage(visible) {
  if (visible) {
    document.getElementById('messagearea').style.display = "";
  } else {
    document.getElementById('messagearea').style.display = "none";
  }
}

function show_hide_obj()
{
  var args = show_hide_obj.arguments;
  var obj = document.getElementById(args[0]);
	
  if (args.length == 2){
    obj.style.display = args[1];
  } else {
	  if(obj.style.display == "none"){
		obj.style.display = "";
	  }else{
		obj.style.display = "none";
	  }
  }
}

function xfindObj(n, d) {
  var p,i,x;  if(!d) d=document; if((p=n.indexOf("?"))>0&&parent.frames.length) {
    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}
  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];
  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=xfindObj(n,d.layers[i].document);
  if(!x && d.getElementById) x=d.getElementById(n); return x;
}

function xvalidateForm() {
  var i,p,q,nm,test,num,min,max,errors='',args=xvalidateForm.arguments;
  for (i=0; i<(args.length-2); i+=3) { test=args[i+2]; val=xfindObj(args[i]);
    if (val) { nm=val.title; if ((val=val.value)!="") {
      if (test.indexOf('isEmail')!=-1) { p=val.indexOf('@');
        if (p<1 || p==(val.length-1)) errors+='- '+nm+' must contain an e-mail address.\n';
      } else if (test!='R') { num = parseFloat(val);
        if (isNaN(val)) errors+='- '+nm+' must contain a number.\n';
        if (test.indexOf('inRange') != -1) { p=test.indexOf(':');
          min=test.substring(8,p); max=test.substring(p+1);
          if (num<min || max<num) errors+='- '+nm+' must contain a number between '+min+' and '+max+'.\n';
    } } } else if (test.charAt(0) == 'R') errors += '- '+nm+' is required.\n'; }
  } if (errors) alert('The following error(s) occurred:\n'+errors);
  document.MM_returnValue = (errors == '');
}

function comparev(val1, val2) {
  var v1, v2, errors='';
  v1 = xfindObj(val1);
  v2 = xfindObj(val2);
  if (v1.value != v2.value){ 
    errors = "Passwords don't match";
	alert('The following error(s) occurred:\n'+errors);
  }
  return (errors == '');
}

function xconfdel(){
  return confirm('Are you sure that you want to remove this item?');
}

function xValidateDate(obj){
  var re = /^(\d{4})-(\d{1,2})-(\d{1,2})$/;
  var result = (obj.value == '') || (
	(re.exec(obj.value)) && 
	(RegExp.$1 > 2000) && 
	(RegExp.$2 >= 1) && (RegExp.$2 <= 12) && 
	(RegExp.$3 >= 1) && (RegExp.$3 <= 31)) ;
  if (result) {
    obj.style.border = "";
    return true
  } else {
    obj.style.border = "2px solid #FF0000";
    alert('Invalid date format. Please use "YYYY-MM-DD" format for date field.');
	obj.focus();
	obj.select();
    return false;
  }
}

function xJumpMenu(targ,selObj,restore){
  eval(targ+".location='"+selObj.options[selObj.selectedIndex].value+"'");
  if (restore) selObj.selectedIndex=0;
}
