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

  function createStatsSidebar() {
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
      var repos = stats_json.allRepos.map(function(repo) {
	var matchingLanguages = repo.languages.filter(function(repoLang) {
          return repoLang.language === lang.language;
	});
	var newRepo = JSON.parse(JSON.stringify(repo)); // safe since repo is a simple json obj.
	newRepo.languages = matchingLanguages;
	return newRepo;
      });
      repos = repos.filter(function(repo) {
	return repo.languages.length > 0;
      });
      repos.sort(function(a,b) {
	return b.languages[0].code - a.languages[0].code;
      }); 
      repos.forEach(function(repo) {
	var code = repo.languages[0].code;
	var repoStats = $('<span class="repoStats">');
	repoStats.append($('<a href="'+repo.repo.url+'" class="repoName">').text(repo.repo.name));
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
  }

  function createProjectList() {

  }

  createStatsSidebar();
  createProjectList();
});
