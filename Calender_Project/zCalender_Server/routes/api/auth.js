const express = require('express');
const mongoose = require('mongoose');
const router = express.Router();

//serving static files (like css)
router.use(express.static('D:/Microsoft VS Code/zCalender_Server/routes/public/login_page'));

//render file 
router.get("/login", (req, res) => {
    res.render("login_page/index");
});

router.get("/register", (req, res) => {
    res.render("register_page/index");
});

const User = require('../../models/User');

router.post('/login', (req, res) => {
    if(req.body.email === undefined){
        res.redirect('register');
    }else{
        console.log(req.body);
    }
});

router.post('/register', (req, res) => {
    if(req.body.password1 !== req.body.password2) console.log('nap');
    
})


//exporting this whole file to main server
module.exports = router;