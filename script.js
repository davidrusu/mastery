$(document).ready(function() {
    var locGoal = 10000;
    
    function progressBar(curr, max, color) {
        var progressOuter = $('<div class="progressOuter">');
        var progressInner = $('<div class="progressInner">');
        var percent = Math.min(curr, max) / max;
        progressInner.width(percent * 100 + '%');
        progressInner.css('background-color', color);
        progressOuter.append(progressInner);
        return progressOuter;
    }
    stats_json.allLanguages.sort(function (a, b) {
        return b.code - a.code;
    });
    stats_json.allLanguages.forEach(function(lang) {
        var langDiv = $('<div class="lang">');
        var langHeader = $('<span class="langHead">');
        langHeader.append($('<div class="langName">').text(lang.language));
        langHeader.append($('<div class="langStats">').text(lang.code + '/' + locGoal));
        langDiv.append(langHeader);
        var c = github_code_colors[lang.language.toLowerCase()];
        var color =  c ? c : '#AAAAAA';
        langDiv.append(progressBar(lang.code, locGoal, color));
        $("#container").append(langDiv);
        var allRepoStats = $('<div class="perRepoStats">');
        var repos = stats_json.allRepos.filter(function(repo) {
            var matchingLanguages = repo.languages.filter(function(repoLang) {
                return repoLang.language === lang.language;
            });
            return matchingLanguages.length > 0;
        });
        console.log(lang.language, repos);
        repos.forEach(function(repo) {
            var matchingLanguages = repo.languages.filter(function(repoLang) {
                return repoLang.language == lang.language;
            });
            var code = matchingLanguages[0].code;
            var repoStats = $('<span class="repoStats">');
            repoStats.append($('<div class="repoName">').text(repo.repo.name));
            repoStats.append($('<div class="repoCode">').text(code));
            repoStats.append($('<div class="repoProgress">').append(progressBar(code, lang.code, color)));
            allRepoStats.append(repoStats);
        });
        langDiv.append(allRepoStats);
        allRepoStats.hide();
        langDiv.click(function() {
            console.log('clicked', lang.language);
            allRepoStats.toggle();
        });
    });
});
