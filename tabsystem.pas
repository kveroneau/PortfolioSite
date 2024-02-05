unit TabSystem;

{$mode objfpc}

interface

uses
  Classes, SysUtils, bulma, JS, Web, Widgets;

var
  TabSys: TBulmaTabs;
  WelcomeTab: TBulmaTab;
  AboutTab: TBulmaTab;
  MyDriveTab: TBulmaTab;
  ProjectsTab: TBulmaTab;
  SkillsTab: TBulmaTab;
  JobsTab: TBulmaTab;
  AboutSiteTab: TBulmaTab;
  ContactTab: TBulmaTab;

procedure InitTabs(ShowWelcome: THTMLClickEventHandler);

implementation

procedure InitTabs(ShowWelcome: THTMLClickEventHandler);
begin
  TabSys:=TBulmaTabs.Create(Nil, 'SectionTabs');
  {WelcomeTab:=TabSys.AddTab('Welcome', 'WelcomeTab', ShowWelcome);}
  AboutTab:=TabSys.AddTab('About Me', 'AboutTab', @AboutMe);
  MyDriveTab:=TabSys.AddTab('My Drive', 'MyDriveTab', @ShowMyDrive);
  ProjectsTab:=TabSys.AddTab('Projects', 'ProjectsTab', @ShowProjects);
  SkillsTab:=TabSys.AddTab('Skills', 'SkillsTab', @ShowSkills);
  JobsTab:=TabSys.AddTab('Jobs', 'JobsTab', @ShowJobs);
  {ContactTab:=TabSys.AddTab('Contact Me', 'ContactTab', @ContactMe);}
  TabSys.renderHTML;
end;

end.

