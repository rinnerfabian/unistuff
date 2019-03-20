const express = require('express');
const mongoose = require('mongoose');
const bodyparser = require('body-parser');
const path = require("path");

const app = express();

//config to use body in post request 
app.use(bodyparser.urlencoded({extended: false}));
app.use(bodyparser.json());

//outsourcing functionality
const auth = require('./routes/api/auth');

//config project for ejs
app.set("views", path.join(__dirname, "/routes/public/"));
app.set("view engine", "ejs");

//Startingpage
app.get('/', (req, res) => {
    res.redirect('/auth/login');
});

//mongoDB configuartion from /setup/myurl
const db = require('./setup/myurl').mongoURL;

//attempt to connect to databse
mongoose
    .connect(db)
    .then(() => console.log('MongoDB connected successfully'))
    .catch(err => console.log(err));    

app.use('/auth', auth);

app.listen(8080, () => console.log('Server is running at port: ' + 8080));