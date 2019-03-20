var nrMonth;
var months = ['Januar','Februar','März','April','Mai','Juni','Juli','August','September','Oktober','November','Dezember'];
var monthdays = [31,28,31,30,31,30,31,31,30,31,30,31];
var date = new Date();
var displayedYear;
var weekday;
var day;
var start;
var ende;
var month;
var year;
var displayedMonth;

function clicked(id){
    var d = document.getElementById(id);
    if (d.className === "" || d.className === "current-day event"){
        d = d.innerHTML;
        console.log(`${d < 10 ? '0' + d : d} ${(nrMonth + 1) < 10 ? '0' + (nrMonth + 1) : (nrMonth + 1)} ${displayedYear}`);
    }
}

function init(){
    nrMonth = date.getMonth();
    month = date.getMonth();
    year = date.getFullYear();
    displayedYear = date.getFullYear();
    weekday = date.getDay();
    day = date.getDate();
    displayedMonth = nrMonth;
    document.getElementById('month').innerHTML = months[nrMonth] + " " + displayedYear;
    getStart(day,weekday);
    setCalendar();
    auffuellen();
}

function nextMonth(){
    if (nrMonth === 11){
        displayedYear ++;
    }
    nrMonth = (nrMonth + 1)%12;
    document.getElementById('month').innerHTML = months[nrMonth] + " " + displayedYear;
    start = (ende+1)%7;
    setCalendar();
    auffuellen();
}

function prevMonth(){
    if (nrMonth === 0) {
        nrMonth = 11;
        displayedYear --;
    }
    else {
        nrMonth = (nrMonth - 1)%12;
    }
    document.getElementById('month').innerHTML = months[nrMonth] + " " + displayedYear;
    var i;
    if (start === 0){
        i = 6;
    }
    else {
        i = (start-1)%7;
    }
    var m = monthdays[nrMonth];
    if (m === 28 && displayedYear%4 === 0){
        m = 29;
    } 
    getStart(m,i);
    setCalendar();
    auffuellen();
}

function getStart(d,w){
    var i = d;
    var j = w;
    while(1){
        if (i === 1){
            break;
        }
        if (j === 0){
            j = 6;
        }
        else {
            j = (j-1)%7;
        }
        i--;
    }
    start = j;
}

function setCalendar(){ 
    for (var x = 0; x < 42;x++){
        document.getElementsByTagName('td')[7+x].innerHTML = "";
        document.getElementsByTagName('td')[7+x].className = "";
    }
    var m = monthdays[nrMonth];
    if (m === 28 && displayedYear%4 === 0){
        m = 29;
    }
    var i = 1;
    for (; i <= m; i++){
        document.getElementsByTagName('td')[6+start+i].innerHTML = i;
        if (i === day && month === nrMonth && year === displayedYear){
            document.getElementsByTagName('td')[6+start+i].className = "current-day event";
        }
        if (i === m){
            ende = (6+start+i)%7;
        }
    }
    
    var s = 1;
    for (;i + start <= 42;i++){
        document.getElementsByTagName('td')[6+i+start].innerHTML = s;
        document.getElementsByTagName('td')[6+i+start].className = "next-month";
        s++;
    }
    
}

function auffuellen(){
    var n = nrMonth;
    if (n === 0){
        n = 11;
    }
    else {
        n = n - 1;
    }
    var m = monthdays[n];
    if (m === 28 && displayedYear%4 === 0){
        m = 29;
    }
    var i = start - 1;
    var c = 0;
    for (;i >= 0;i--){
        document.getElementsByTagName('td')[7+i].innerHTML = m - c;
        document.getElementsByTagName('td')[7+i].className = "prev-month";
        c++;
    }

    
}